use std::io::Write;

use console::{Key, Term, style};

pub struct EditorContext {
    prompt: Option<String>,
    should_exit: bool,
}

impl EditorContext {
    fn new() -> Self {
        Self {
            prompt: None,
            should_exit: false,
        }
    }

    pub fn set_prompt(&mut self, prompt: String) {
        self.prompt = Some(prompt);
    }

    pub fn exit(&mut self) {
        self.should_exit = true;
    }
}

pub trait EditorEventHandler {
    fn handle_input(&mut self, line: String, ctx: &mut EditorContext);

    fn handle_hint(&mut self, input: String) -> Option<Vec<String>>;

    fn handle_completion(&mut self, input: String) -> Option<Vec<String>>;
}

pub struct Editor {
    term: Term,
    buffer: Vec<char>,
    buffer_pos: usize,
    prompt: String,

    current_hint: Option<String>,
    hide_hint: bool,
    no_hint_count: usize,

    handler: Box<dyn EditorEventHandler>,

    history: Vec<String>,
    history_pos: usize,
}

impl Editor {
    pub fn new(handler: Box<dyn EditorEventHandler>) -> Self {
        Self {
            term: Term::stdout(),
            buffer: Vec::new(),
            buffer_pos: 0,
            prompt: "> ".to_string(),
            current_hint: None,
            hide_hint: false,
            no_hint_count: 0,
            handler,
            history: Vec::new(),
            history_pos: 0,
        }
    }

    fn get_cursor_pos(&self) -> usize {
        let s: String = self.buffer.iter().take(self.buffer_pos).collect();
        console::measure_text_width(&s)
    }

    fn redraw_line(&self) -> Result<(), std::io::Error> {
        self.term.clear_line()?;
        write!(&self.term, "{}", self.prompt)?;
        let line = self.get_buffer_string();
        write!(&self.term, "{}", line)?;
        if let Some(hint) = &self.current_hint {
            write!(&self.term, "{}", style(hint).dim())?;
            let offset = console::measure_text_width(hint);
            self.term.move_cursor_left(offset)?;
        }
        let cursor_pos = self.get_cursor_pos();
        self.term
            .move_cursor_left(console::measure_text_width(&line) - cursor_pos)?;
        Ok(())
    }

    fn get_buffer_string(&self) -> String {
        self.buffer.iter().collect()
    }

    pub fn read_lines(&mut self) {
        loop {
            match self.read_line() {
                Ok(exit) => {
                    let line = self.get_buffer_string();
                    if !line.trim().is_empty() {
                        self.history.push(line.clone());
                        self.history_pos = self.history.len();
                    }
                    if exit {
                        break;
                    }
                    if !self.buffer.is_empty() {
                        let mut ctx = EditorContext::new();
                        self.handler.handle_input(line, &mut ctx);
                        if let Some(prompt) = ctx.prompt {
                            self.prompt = prompt;
                        }
                        if ctx.should_exit {
                            break;
                        }
                    }
                }
                Err(err) => {
                    eprintln!("{}", err);
                    break;
                }
            }
        }
    }

    fn handle_hint(&mut self) -> Result<(), std::io::Error> {
        self.current_hint = None;
        if !self.buffer.is_empty() {
            let hints = self.handler.handle_hint(self.get_buffer_string());
            if let Some(hints) = hints
                && !hints.is_empty()
            {
                let hint = longest_common_prefix(&hints);
                self.current_hint = Some(hint.to_string());
            }
        }
        Ok(())
    }

    fn apply_hint(&mut self) -> Result<(), std::io::Error> {
        if let Some(hint) = &self.current_hint {
            self.buffer.extend(hint.chars());
            self.buffer_pos = self.buffer.len();
            self.current_hint = None;
        } else {
            self.no_hint_count += 1;
            let line = self.get_buffer_string();
            if let Some(completions) = self.handler.handle_completion(line)
                && !completions.is_empty()
            {
                for (i, completion) in completions.iter().enumerate() {
                    if i > 0 {
                        write!(self.term, " ")?;
                    }
                    write!(self.term, "{}", completion)?;
                }
                write!(self.term, "\n")?;
            }
        }
        Ok(())
    }

    fn read_line(&mut self) -> Result<bool, std::io::Error> {
        let mut exit = false;

        let prompt = "> ";
        write!(self.term, "{}", prompt)?;
        self.buffer.clear();
        self.buffer_pos = 0;

        loop {
            if !self.hide_hint {
                self.handle_hint()?;
            }

            self.redraw_line()?;

            let key = self.term.read_key_raw()?;
            match key {
                Key::Char(ch) => {
                    // eprintln!("{:?}", key);
                    if ch == '\u{4}' && self.buffer.is_empty() {
                        // Ctrl-D
                        println!();
                        exit = true;
                        break;
                    }
                    if ch == '\u{c}' {
                        // Ctrl-L
                        self.term.clear_screen()?;
                        write!(self.term, "\x1B[3J")?; // Clear the scrollback buffer
                        write!(self.term, "{}", prompt)?;
                        self.buffer_pos = 0;
                    }
                    if ch == '\u{15}' {
                        // Ctrl-U
                        if self.buffer_pos > 0 {
                            self.buffer.drain(..self.buffer_pos);
                            self.buffer_pos = 0;
                        }
                    }
                    if ch == '\u{b}' {
                        // Ctrl-K
                        if self.buffer_pos < self.buffer.len() {
                            self.buffer.drain(self.buffer_pos..);
                        }
                    }
                    if ch.is_control() {
                        continue;
                    }
                    self.buffer.insert(self.buffer_pos, ch);
                    self.buffer_pos += 1;
                    self.hide_hint = false;
                }
                Key::CtrlC => {
                    println!();
                    self.buffer.clear();
                    self.buffer_pos = 0;
                    break;
                }
                Key::Backspace => {
                    if self.buffer_pos > 0 {
                        self.buffer.remove(self.buffer_pos - 1);
                        self.buffer_pos -= 1;
                    }
                }
                Key::ArrowLeft => {
                    if self.buffer_pos > 0 {
                        self.buffer_pos -= 1;
                    }
                }
                Key::ArrowRight => {
                    if self.buffer_pos < self.buffer.len() {
                        self.buffer_pos += 1;
                    } else {
                        self.apply_hint()?;
                    }
                }
                Key::ArrowUp => {
                    self.search_history().map(|content| {
                        self.buffer = content.chars().collect();
                        self.buffer_pos = self.buffer.len();
                    });
                }
                Key::ArrowDown => {
                    self.search_history_rev().map(|content| {
                        self.buffer = content.chars().collect();
                        self.buffer_pos = self.buffer.len();
                    });
                }
                Key::Home => {
                    self.buffer_pos = 0;
                }
                Key::End => {
                    self.buffer_pos = self.buffer.len();
                }
                Key::Enter => {
                    write!(self.term, "\n")?;
                    return Ok(false);
                }
                Key::Tab => {
                    self.apply_hint()?;
                }
                Key::Escape => {
                    self.current_hint = None;
                    self.hide_hint = true;
                }
                _ => {}
            }
        }

        Ok(exit)
    }

    fn search_history(&mut self) -> Option<String> {
        if self.history_pos > 0 {
            self.history_pos -= 1;
            return Some(self.history[self.history_pos].clone());
        }
        None
    }

    fn search_history_rev(&mut self) -> Option<String> {
        if self.history_pos < self.history.len() - 1 {
            self.history_pos += 1;
            return Some(self.history[self.history_pos].clone());
        }
        None
    }
}

fn common_prefix<'a>(s1: &'a str, s2: &'a str) -> &'a str {
    let len = s1
        .chars()
        .zip(s2.chars())
        .position(|(a, b)| a != b)
        .unwrap_or(s1.len());
    &s1[..len]
}

fn longest_common_prefix<'a>(strings: &'a [String]) -> &'a str {
    if strings.is_empty() {
        return "";
    }

    let mut prefix: &str = &strings[0];
    for s in strings.iter().skip(1) {
        prefix = common_prefix(&prefix, s);
    }
    prefix
}
