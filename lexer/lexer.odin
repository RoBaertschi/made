package lexer

/*
Alot of this code is copied and modified over from https://github.com/odin-lang/Odin/blob/master/core/odin/tokenizer/token.odin
It is distributed under the 3-clause BSD License. Following is the LICENSE:


Copyright (c) 2016-2024 Ginger Bill. All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

import "core:fmt"
import "core:strings"
import "core:unicode/utf8"

Token_Type :: enum {
	// Literals
	Identifier,
	Command,
	File,

	// Punctuation
	OpenBrace,
	CloseBrace,
	Arrow,
	Colon,

	// Signal Tokens
	Invalid,
	EOF,
}


Token :: struct {
	type: Token_Type,
	loc:  Loc,
	data: union {
		// Identifier, Command, File
		string,
	},
}

Loc :: struct {
	file:   string,
	line:   int,
	column: int,
}

Error_Callback :: #type proc(loc: Loc, fmt: string, args: ..any)

Lexer :: struct {
	// Immutable
	input:          string,
	file:           string,
	error_callback: Error_Callback,

	// State
	ch:             rune,
	pos:            int,
	peek_pos:       int,
	last_line_pos:  int,
	line:           int,

	// Mutable
	errors:         int,
}

default_error_callback :: proc(loc: Loc, format: string, args: ..any) {
	fmt.eprintf("%s(%d:%d) ", loc.file, loc.line, loc.column)
	fmt.eprintf(format, ..args)
	fmt.eprintf("\n")
}

init :: proc(
	l: ^Lexer,
	input: string,
	file: string,
	error_callback: Error_Callback = default_error_callback,
) {
	l.input = input
	l.file = file
	l.error_callback = error_callback

	l.ch = ' '
	l.pos = 0
	l.peek_pos = 0
	l.line = len(input) > 0 ? 1 : 0
	l.last_line_pos = 0
	l.errors = 0

	next_ch(l)
	if l.ch == utf8.RUNE_BOM {
		next_ch(l)
	}
}

@(private)
pos_to_loc :: proc(l: ^Lexer, pos: int) -> Loc {
	return Loc{file = l.file, line = l.line, column = pos - l.last_line_pos + 1}
}

error :: proc(l: ^Lexer, pos: int, format: string, args: ..any) {
	loc := pos_to_loc(l, pos)
	if l.error_callback != nil {
		l.error_callback(loc, format, ..args)
	}
	l.errors += 1
}

next_ch :: proc(l: ^Lexer) {
	if l.peek_pos < len(l.input) {
        l.pos = l.peek_pos
		if l.ch == '\n' {
			l.last_line_pos = l.pos
			l.line += 1
		}
		r, w := rune(l.input[l.peek_pos]), 1
		switch {
		case r == 0:
			error(l, l.pos, "illegal NUL character encountered")
		case r >= utf8.RUNE_SELF:
			r, w = utf8.decode_rune_in_string(l.input[l.peek_pos:])
			if r == utf8.RUNE_ERROR && w == 1 {
				error(l, l.pos, "illegal UTF-8 character")
			} else if r == utf8.RUNE_BOM && l.pos > 0 {
				error(l, l.pos, "illegal byte order mark")
			}
		}
		l.peek_pos += w
		l.ch = r
	} else {
		l.pos = len(l.input)
		if l.ch == '\n' {
			l.last_line_pos = l.pos
			l.line += 1
		}
		l.ch = -1
	}
}

peek_byte :: proc(l: ^Lexer) -> byte {
    if l.peek_pos < len(l.input) {
        return l.input[l.peek_pos]
    }
    return 0
}

next_token :: proc(l: ^Lexer) -> Token {
	type := Token_Type.Invalid
	loc := pos_to_loc(l, l.pos)

    switch l.ch {
    case -1: type = .EOF
    case '=': 
        if peek_byte(l) == '>' {
            next_ch(l)
            type = .Arrow
        }
    case '{': type = .OpenBrace
    case '}': type = .CloseBrace
    case ':': type = .Colon
    case:
        
    }
    next_ch(l)
	return Token{
        loc = loc,
        type = type,
        data = nil,
    }
}
