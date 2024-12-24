package parser

import "../ast"
import "../lexer"
import "core:fmt"
import "core:strings"
import "core:mem"

// A hard error, something that should make the program clean and exit, this does not indicate a parser error but a other runtime error
Error :: union {
    mem.Allocator_Error,
}

Error_Callback :: #type proc(offending_token: lexer.Token, fmt: string, args: ..any)

default_error_callback :: proc(offending_token: lexer.Token, format: string, args: ..any) {
	loc := offending_token.loc
	fmt.eprintf("%s(%d:%d) Parser Error: ", loc.file, loc.line, loc.column)
	fmt.eprintf(format, ..args)
	fmt.eprintf("\n")
}

Parser :: struct {
	// Immutable
	l:              lexer.Lexer,

	// State
	cur_token:      lexer.Token,
	peek_token:     lexer.Token,
	error_callback: Error_Callback,

	// Mutable
	errors:         int,
}

init :: proc(p: ^Parser, l: lexer.Lexer, error_callback := default_error_callback) {
	p.l = l
	p.errors = 0
	p.error_callback = error_callback
	next_token(p)
	next_token(p)
}

destroy :: proc(p: ^Parser) {
    lexer.destroy_token(p.cur_token)
    lexer.destroy_token(p.peek_token)
}


@(private)
next_token :: proc(p: ^Parser) {
    lexer.destroy_token(p.cur_token)
	p.cur_token = p.peek_token
	p.peek_token = lexer.next_token(&p.l)
}

error :: proc(p: ^Parser, format: string, args: ..any) {
	if p.error_callback != nil {
		p.error_callback(p.cur_token, format, ..args)
	}
	p.errors += 1
}

expect_peek :: proc(p: ^Parser, type: lexer.Token_Type) -> bool {
	return expect_peekf(p, type, "Expected \"%v\", got \"%v\".", type, p.peek_token.type)
}

expect_peekf :: proc(p: ^Parser, type: lexer.Token_Type, format := "", args: ..any) -> bool {
	if p.peek_token.type == type {
		next_token(p)
		return true
	}
	error(p, format, ..args)
	return false
}

expect_peek_string :: proc(p: ^Parser) -> (str: string, ok: bool, err: Error) {
	if p.peek_token.type == .Identifier || p.peek_token.type == .File {
		next_token(p)
		clone := strings.clone(p.cur_token.data.(string)) or_return
        return clone, true, nil
	}
	error(
		p,
		"Expected a \"%v\" or \"%v\", but got \"%v\"",
		lexer.Token_Type.File,
		lexer.Token_Type.Identifier,
		p.peek_token.type,
	)
	return "", false, nil
}

parse_command :: proc(p: ^Parser, identifier: string) -> (stmt: Maybe(ast.Command_Statement), err: Error) {
	if !expect_peek(p, .OpenBrace) {
        delete(identifier)
		return
	}

	commands := make([dynamic]string) or_return
	if !expect_peek(p, .Command) {
        delete(identifier)
        delete(commands)
		return
	}

	append(&commands, strings.clone(p.cur_token.data.(string)) or_return) or_return

	for p.peek_token.type == .Command {
		next_token(p)
		append(&commands, strings.clone(p.cur_token.data.(string)) or_return) or_return
	}

	if !expect_peek(p, .CloseBrace) {
        delete(identifier)
        for c in commands {
            delete(c)
        }
        delete(commands)
		return
	}

    next_token(p)

	return ast.Command_Statement{identifier = identifier, commands = commands}, nil
}

parse_build :: proc(p: ^Parser, input_file: string) -> (stmt: Maybe(ast.Build_Statement), err: Error) {
	if !expect_peek(p, .Arrow) {
        delete(input_file) or_return
		return
	}

	output_file, ok := expect_peek_string(p) or_return
	if !ok {
        delete(input_file) or_return
        delete(output_file) or_return
		return
	}

	if !expect_peek(p, .Colon) {
        delete(input_file) or_return
        delete(output_file) or_return
		return
	}

	if !expect_peek(p, .Identifier) {
        delete(input_file) or_return
        delete(output_file) or_return
        return
    }

    rule_name := strings.clone(p.cur_token.data.(string)) or_return

    next_token(p)

	return ast.Build_Statement {
		input_file = input_file,
		output_file = output_file,
		rule_name = rule_name,
	}, nil
}

parse_statement :: proc(p: ^Parser) -> (stmt: Maybe(ast.Statement), err: Error) {
	#partial switch p.cur_token.type {
	case .Identifier:
		#partial switch p.peek_token.type {
		case .OpenBrace:
			command := parse_command(p, strings.clone(p.cur_token.data.(string))) or_return
			if command == nil {
				return
			}
			return ast.Statement(command.(ast.Command_Statement)), nil
		case .Arrow:
			build := parse_build(p, strings.clone(p.cur_token.data.(string))) or_return
			if build == nil {
				return
			}
			return ast.Statement(build.(ast.Build_Statement)), nil
		case:
			return
		}
	case .File:
        build := parse_build(p, strings.clone(p.cur_token.data.(string))) or_return
        if build == nil {
            return 
        }
        return ast.Statement(build.(ast.Build_Statement)), nil

    case .EOF:
        return

	case:
		error(p, "could not parse statement based on token type %v", p.cur_token.type)
		return
	}
}

parse_build_file :: proc(p: ^Parser) -> (bf: ast.Build_File, err: Error) {
	bf = ast.Build_File {
		file = strings.clone(p.l.file) or_return,
	}

	statements := make([dynamic]ast.Statement) or_return

    stmt: Maybe(ast.Statement)
	for stmt, err = parse_statement(p); stmt != nil; stmt, err = parse_statement(p) {
        if err != nil {
            for stmt in statements {
                ast.statement_destroy(stmt)
            }
            delete(statements)
            return
        }
		append(&statements, stmt.(ast.Statement)) or_return
	}

	bf.statements = statements

	return
}

import "core:testing"
import "core:log"

@(private)
test_error_callback :: proc(offending_token: lexer.Token, format: string, args: ..any) {
	loc := offending_token.loc

    output := fmt.aprintf("%s(%d:%d) Parser Error: ", loc.file, loc.line, loc.column)
    custom_format := fmt.aprintf(format, ..args)
    log.errorf("%v%v", output, custom_format)
}

@(private)
test_expect_statement :: proc(t: ^testing.T, expected, actual: ast.Statement) {
    switch stmt in expected {
    case ast.Build_Statement:
        act, ok := actual.(ast.Build_Statement)
        testing.expectf(t, ok, "Expected actual to be Build_Statement but got %T", actual)
        if !ok {
            return
        }
        testing.expectf(t, stmt.rule_name == act.rule_name, "Expected rule_name \"%v\", got \"%v\"", stmt.rule_name, act.rule_name)
        testing.expectf(t, stmt.input_file == act.input_file, "Expected input_file \"%v\", got \"%v\"", stmt.input_file, act.input_file)
        testing.expectf(t, stmt.output_file == act.output_file, "Expected output_file \"%v\", got \"%v\"", stmt.output_file, act.output_file)
    case ast.Command_Statement:
        act, ok := actual.(ast.Command_Statement)
        testing.expectf(t, ok, "Expected actual to be Command_Statement but got %T", actual)
        if !ok {
            return
        }
        testing.expectf(t, stmt.identifier == act.identifier, "Expected identifier \"%v\", got \"%v\"", stmt.identifier, act.identifier)
        if !testing.expectf(t, len(stmt.commands) == len(act.commands), "Expected the lenght of commands to be %v, but got %v", len(stmt.commands), len(act.commands)) { return }
        for c, i in stmt.commands {
            testing.expectf(t, c == act.commands[i], "Expected command %v to be equal to \"%v\", but got \"%v\"", i, c, act.commands[i])
        }
    }
}

@(test)
test_basic_build :: proc(t: ^testing.T) {
    input := " main.c => main: gcc"
    l := lexer.Lexer{}
    lexer.init(&l, input, "Madefile")

    p := Parser{}
    init(&p, l, test_error_callback)
    defer destroy(&p)

    build_file, err := parse_build_file(&p)

    testing.expectf(t, err == nil, "Error occured when parsing build file, err: %v", err)
    if err != nil {
        return
    }
    defer ast.build_file_destroy(build_file)

    testing.expectf(t, p.errors == 0, "Expected to have 0 parser errors, got %v instead.", p.errors)
    testing.expectf(t, len(build_file.statements) == 1, "Expected 1 statement, got %v", len(build_file.statements))

    test_expect_statement(t, ast.Build_Statement{input_file = "main.c", output_file = "main", rule_name = "gcc"}, build_file.statements[0])
}


@(test)
test_command_statement :: proc(t: ^testing.T) {
    input := " gcc {\n\t$gcc -o %o %i\n} "
    l := lexer.Lexer{}
    lexer.init(&l, input, "Madefile")

    p := Parser{}
    init(&p, l, test_error_callback)
    defer destroy(&p)

    build_file, err := parse_build_file(&p)

    testing.expectf(t, err == nil, "Error occured when parsing build file, err: %v", err)
    if err != nil {
        return
    }
    defer ast.build_file_destroy(build_file)

    testing.expectf(t, p.errors == 0, "Expected to have 0 parser errors, got %v instead.", p.errors)
    testing.expectf(t, len(build_file.statements) == 1, "Expected 1 statement, got %v", len(build_file.statements))

    commands := [dynamic]string{"gcc -o %o %i"}
    defer delete(commands)
    test_expect_statement(t, ast.Command_Statement{identifier = "gcc", commands = commands}, build_file.statements[0])
}
