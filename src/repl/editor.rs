use std::io::Write;

use console::{Key, Term, style};
use thiserror::Error;

pub struct EditorContext {
    should_exit: bool,
}

impl EditorContext {
    fn new() -> Self {
        Self { should_exit: false }
    }

    pub fn exit(&mut self) {
        self.should_exit = true;
    }
}

pub trait EditorEventHandler {
    fn handle_input(&mut self, line: String, ctx: &mut EditorContext);

    fn handle_error(&mut self, err: EditorError, ctx: &mut EditorContext);

    fn handle_hint(&mut self, input: String) -> Option<Vec<String>>;

    fn handle_completion(&mut self, input: String) -> Option<Vec<String>>;

    fn get_prompt(&self) -> &str {
        "> "
    }
}

#[derive(Debug, Error)]
pub enum EditorError {
    #[error("Interrupt")]
    Interrupt(String),
    #[error("EOF")]
    Eof,
    #[error("IO Error: {0}")]
    Io(#[from] std::io::Error),
}

pub struct Editor {
    term: Term,
    buffer: Vec<char>,
    buffer_pos: usize,

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
        write!(&self.term, "{}", self.handler.get_prompt())?;
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
            let mut ctx = EditorContext::new();
            match self.read_line() {
                Ok(_) => {
                    let line = self.get_buffer_string();
                    if !line.trim().is_empty() {
                        if self.history.is_empty() || self.history.last().unwrap().ne(&line) {
                            self.history.push(line.clone());
                            self.history_pos = self.history.len();
                        }
                    }
                    self.handler.handle_input(line, &mut ctx);
                }
                Err(err) => {
                    self.handler.handle_error(err, &mut ctx);
                }
            }
            if ctx.should_exit {
                break;
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

    fn apply_hint(&mut self, show_completion: bool) -> Result<(), std::io::Error> {
        if let Some(hint) = &self.current_hint {
            self.buffer.extend(hint.chars());
            self.buffer_pos = self.buffer.len();
            self.current_hint = None;
        } else if show_completion {
            self.no_hint_count += 1;
            let line = self.get_buffer_string();
            if let Some(mut completions) = self.handler.handle_completion(line)
                && !completions.is_empty()
            {
                self.redraw_line()?;
                write!(self.term, "\n")?;
                completions.sort();
                self.print_items(&completions)?;
                write!(self.term, "\n\n")?;
            }
        }
        Ok(())
    }

    fn read_line(&mut self) -> Result<(), EditorError> {
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
                        return Err(EditorError::Eof);
                    }
                    if ch == '\u{c}' {
                        // Ctrl-L
                        self.term.clear_screen()?;
                        write!(self.term, "\x1B[3J")?; // Clear the scrollback buffer
                        write!(self.term, "{}", self.handler.get_prompt())?;
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
                    let line = self.get_buffer_string();
                    self.buffer.clear();
                    self.buffer_pos = 0;
                    return Err(EditorError::Interrupt(line));
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
                        self.apply_hint(false)?;
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
                    break;
                }
                Key::Tab => {
                    self.apply_hint(true)?;
                }
                Key::Escape => {
                    self.current_hint = None;
                    self.hide_hint = true;
                }
                _ => {}
            }
        }

        Ok(())
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

    fn print_items<'a>(&self, items: &'a [String]) -> Result<(), std::io::Error> {
        if items.is_empty() {
            return Ok(());
        }
        let longest_width = items
            .iter()
            .map(|s| console::measure_text_width(s))
            .max()
            .unwrap();
        let columns = self.term.size().1 as usize;
        let count_per_row = std::cmp::max(columns / (longest_width + 2), 1);
        for (i, item) in items.iter().enumerate() {
            if i % count_per_row == 0 {
                write!(&self.term, "\n")?;
            }
            write!(&self.term, "{}", item)?;
            write!(
                &self.term,
                "{}  ",
                " ".repeat(longest_width - console::measure_text_width(item))
            )?;
        }
        Ok(())
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
