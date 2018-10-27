use std::str::CharIndices;
use ast::{Op};

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

#[derive(PartialEq, Debug)]
pub enum Token {
    Num(i64),
    Op(Op),
    LParen,
    RParen
}
#[derive(Debug)]
pub enum LexicalError {
    UnknownChar((usize, char))
}

pub struct Lexer<'input> {
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>
}


impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut chars = input.char_indices();
        let lookahead = chars.next();
        Lexer { chars, lookahead }
    }
    
    fn bump(&mut self) -> Option<(usize, char)> {
        let next = self.lookahead;
        self.lookahead = self.chars.next();
        next
    }

    fn skip_whitespace(&mut self) {
        while let Some((_i, ch)) = self.lookahead {
            if !ch.is_whitespace() {
                return;
            } else {
                self.bump();
            }
        }
    }

    fn read_number(&mut self, start_location: usize, first_char: char) -> <Lexer<'input> as Iterator>::Item {
        let mut out = first_char.to_string();
        loop {
            match self.lookahead {
                Some((_i, c)) if c.is_ascii_digit() => {
                    out.push(c);
                    self.bump();
                }
                _ => return Ok((start_location,
                             Token::Num(out.parse::<i64>().expect("Unparseable number")),
                             start_location + out.len()))
            }
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        self.skip_whitespace();
        if let Some((i, c)) = self.bump() {
            match c {
                c if c.is_ascii_digit() => Some(self.read_number(i, c)),
                '+' => Some(Ok((i, Token::Op(Op::Plus), i + 1))),
                '-' => Some(Ok((i, Token::Op(Op::Minus), i + 1))),
                '*' => Some(Ok((i, Token::Op(Op::Times), i + 1))),
                '/' => Some(Ok((i, Token::Op(Op::Div), i + 1))),
                '(' => Some(Ok((i, Token::LParen, i + 1 ))),
                ')' => Some(Ok((i, Token::RParen, i + 1 ))),
                c => Some(Err(LexicalError::UnknownChar((i, c))))
            }
        } else {
            None
        }
    }
        
}
