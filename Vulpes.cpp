//---Program Options
#define enable_lexer_print 1
#define enable_parser_print 1
#define enable_linearizer_print 1

//---Includes
#include "foxlib.hpp"

LinearAllocator heap_stack;

//---SourceSite
struct SourceSite {
	ConstString path;
	String source;
	u64 line;
	char* line_start;
	u64 column;
};

template<typename AllocatorType>
void write(String* buffer, SourceSite site, AllocatorType* allocator) {
	write(buffer, "File: ", allocator);
	write(buffer, site.path, allocator);
	write(buffer, " line ", allocator);
	write_uint(buffer, site.line, allocator);
	write(buffer, " column ", allocator);
	write_uint(buffer, site.column, allocator);
	write(buffer, "\n", allocator);
	
	//print the line
	//@todo print context
	//@todo print error as range rather than just start
	auto line_end = site.line_start;
	auto source_end = site.source.data + site.source.length;
	while (line_end < source_end && *line_end != '\n') {
		line_end++;
	}
	
	String line;
	zero(&line);
	line.data = site.line_start;
	line.length = line_end - site.line_start;
	fox_for (source_index, line.length) {
		if (line[source_index] == '\t') {
			write(buffer, "  ", allocator);
		} else {
			write_char(buffer, line[source_index], allocator);
		}
	}
	write(buffer, "\n", allocator);
	
	//print a cursor to the start of the site
	//-------------------------------^
	fox_for (source_index, site.column - 1) {
		if (line[source_index] == '\t') {
			write(buffer, "--", allocator);
		} else {
			write(buffer, "-", allocator);
		}
	}
	write(buffer, "^", allocator);
	write(buffer, "\n", allocator);
}

//---Diagnostics
template<typename AllocatorType>
void write_indent(String* buffer, fuint indent, AllocatorType* allocator) {
	fox_for (_, indent) {
		write(buffer, "  ", allocator);
	}
}

//Print unexpected end of file at <where> error message
//Where should end in a line ending
void print_unexpected_end_of_file(SourceSite site, ConstString where) {
	fox_assert(where[where.length - 1] == '\n');
	
	String error_message;
	zero(&error_message);
	write(&error_message, site, &heap_stack);
	write(&error_message, "Syntax error: unexpected end of file ", &heap_stack);
	write(&error_message, where, &heap_stack);
	print(error_message);
}

//---Lexer
bool is_space_character(char* character) {
	return *character <= ' ';
}

bool is_operator_character(char* character) {
	return find(ConstString("`~!@#$%^&*()-+=[]\\{}|;:,./"), *character);
}

bool is_number_character(char* character) {
	return '0' <= *character && *character <= '9';
}

bool is_identifier_character(char* character) {
	return !is_space_character(character)
		&& !is_operator_character(character);
}

//Language keywords/operators 
//Aligns with keyword_strings;
enum class Keyword {
	assign,
	multiply,
	divide,
	add,
	subtract,
	declare,
	function,
	integer,
	comma,
	open_parenthesis,
	close_parenthesis,
	open_curly,
	close_curly,
	keyword_count
};

ConstString keyword_strings[] = {
	"=",
	"*",
	"/",
	"+",
	"-",
	":",
	"function",
	"int",
	",",
	"(",
	")",
	"{",
	"}",
};

u64 precedences[] = {
	100, // =
	300, // *
	300, // /
	200, // +
	200, // -
	150, // :
	0,   // function
	0,   // int
	0,   // ,
	0,   // (
	0,   // )
	0,   // {
	0,   // }
};

static_assert((fuint)Keyword::keyword_count == fox_array_length(keyword_strings)
			  && (fuint)Keyword::keyword_count == fox_array_length(precedences));

enum class Associativity : u8 {
	left,
	right,
};

Associativity get_associativity(Keyword keyword) {
	if (keyword == Keyword::assign) {
		return Associativity::right;
	}
	return Associativity::left;
}

enum class TokenKind : u8 {
	invalid,
	keyword,
	integer,
	string,
	identifier,
};

struct Token {
	SourceSite site;
	union {
		Keyword keyword_value;
		ConstString identifier_value;
		ConstString string_value;
		u64 integer_value;
	};
	TokenKind kind;
};

template<typename AllocatorType>
void write(String* buffer, Keyword keyword, AllocatorType* allocator) {
	write(buffer, keyword_strings[(fuint)keyword], allocator);
}

template<typename AllocatorType>
void write(String* buffer, Token token, AllocatorType* allocator) {
	switch (token.kind) {
		case TokenKind::keyword: {
			write(buffer, "Keyword (", allocator);
			write(buffer, token.keyword_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::integer: {
			write(buffer, "Integer (", allocator);
			write_uint(buffer, token.integer_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::string: {
			write(buffer, "String (", allocator);
			write(buffer, token.string_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::identifier: {
			write(buffer, "Identifier (", allocator);
			write(buffer, token.identifier_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

Optional<Array<Token>> lex(String source, ConstString path) {
	fox_assert(source);
	
	Array<Token> tokens;
	zero(&tokens);
	
	//Replace '\r\n' with ' \n'
	fox_for (source_index, source.length) {
		if (source[source_index] == '\r') {
			if (source_index + 1 < source.length && source[source_index + 1] == '\n') {
				source[source_index] = ' ';
				source_index++;
			} else {
				source[source_index] = '\n';
			}
		}
	}
	
	auto cursor = source.data;
	auto source_end = source.data + source.length;
	SourceSite site_cursor;
	zero(&site_cursor);
	site_cursor.path = path;
	site_cursor.source = source;
	site_cursor.line = 1;
	site_cursor.line_start = source.data;
	site_cursor.column = 1;
	
	auto increment_cursor = [&]() {
		cursor++;
		site_cursor.column++;
	};
	
	auto add_to_cursor = [&](fuint amount) {
		cursor += amount;
		site_cursor.column += amount;
	};
	
	while (cursor < source_end) {
		//Skip whitespace
		while (is_space_character(cursor)) {
			if (*cursor == '\n') {
				cursor++;
				site_cursor.line++;
				site_cursor.line_start = cursor;
				site_cursor.column = 1;
			} else {
				increment_cursor();
			}
			
			if (cursor == source_end) {
				return tokens;
			}
		}
		
		auto push_new_token = [&](TokenKind kind, SourceSite site) {
			auto new_token = push_zero(&tokens, &heap_stack);
			new_token->kind = kind;
			new_token->site = site;
			return new_token;
		};
		
		//Check for operators
		if (is_operator_character(cursor)) {
			auto operator_start_site = site_cursor;
			auto operator_start = cursor;
			
			auto operator_end_cursor = cursor;
			while (operator_end_cursor < source_end && is_operator_character(operator_end_cursor)) {
				operator_end_cursor++;
			}
			
			ConstString operator_string;
			zero(&operator_string);
			operator_string.data = operator_start;
			operator_string.length = operator_end_cursor - operator_start;
			
			auto keyword_found = false;
			while (operator_string) {
				auto keyword_index_optional = find(Array<ConstString>(keyword_strings), operator_string);
				if (keyword_index_optional) {
					auto new_token = push_new_token(TokenKind::keyword, operator_start_site);
					new_token->keyword_value = (Keyword)keyword_index_optional.value;
					add_to_cursor(operator_end_cursor - cursor);
					keyword_found = true;
					break;
				}
				
				operator_string.length--;
				operator_end_cursor--;
			}
			
			if (!keyword_found) {
				String error_message;
				zero(&error_message);
				write(&error_message, operator_start_site, &heap_stack);
				write(&error_message, "Syntax error: ", &heap_stack);
				write(&error_message, operator_string, &heap_stack);
				write(&error_message, " is not a valid operator.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
		
		//Check for numbers
		else if (is_number_character(cursor)) {
			auto number_start_site = site_cursor;
			auto number_start = cursor;
			while (cursor < source_end && is_number_character(cursor)) {
				increment_cursor();
			}
			
			if (cursor == source_end || is_space_character(cursor) || is_operator_character(cursor)) {
				ConstString number_string;
				zero(&number_string);
				number_string.data = number_start;
				number_string.length = cursor - number_start;
				
				u64 integer_value = unsigned_integer_from_string(number_string);
				
				auto new_token = push_new_token(TokenKind::integer, number_start_site);
				new_token->integer_value = integer_value;
			} else {
				String error_message;
				zero(&error_message);
				write(&error_message, site_cursor, &heap_stack);
				write(&error_message, "Syntax error: Expected end-of-file, whitespace, or operator after integer literal.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
		
		//Check for string literals
		else if (*cursor == '\'') {
			auto string_start_site = site_cursor;
			
			increment_cursor();
			
			String string_value;
			zero(&string_value);
			
			while (true) {
				if (*cursor == '\\') {
					if (cursor + 1 == source_end) {
						print_unexpected_end_of_file(site_cursor, "after escape sequence start.\n");
						return nil;
					}
					
					if (is_space_character(cursor + 1)) {
						String error_message;
						zero(&error_message);
						write(&error_message, site_cursor, &heap_stack);
						write(&error_message, "Syntax error: whitespace after escape sequence start.\n", &heap_stack);
						print(error_message);
						return nil;
					}
					
					switch (cursor[1]) {
						case '\\': {
							write(&string_value, "\\", &heap_stack);
						} break;
						
						case 'n': {
							write(&string_value, "\n", &heap_stack);
						} break;
						
						case '\'': {
							write(&string_value, "'", &heap_stack);
						} break;
						
						default: {
							String error_message;
							zero(&error_message);
							write(&error_message, site_cursor, &heap_stack);
							write(&error_message, "Syntax error: unrecognized escape sequence \\", &heap_stack);
							write_char(&error_message, cursor[1], &heap_stack);
							write(&error_message, "\n", &heap_stack);
							print(error_message);
							return nil;
						} break;
					}
					
					cursor += 2;
				} else if (*cursor == '\'') {
					increment_cursor();
					break;
				} else {
					write_char(&string_value, *cursor, &heap_stack);
					increment_cursor();
				}
				
				if (cursor >= source_end) {
					print_unexpected_end_of_file(string_start_site, "inside string.\n");
					return nil;
				}
			}
			
			auto new_token = push_new_token(TokenKind::string, string_start_site);
			new_token->string_value = string_value;
		}
		
		//anything else is considered an identifier character
		else {
			fox_assert(is_identifier_character(cursor));
			
			auto identifier_start_site = site_cursor;
			auto identifier_start = cursor;
			while (cursor < source_end && is_identifier_character(cursor)) {
				increment_cursor();
			}
			
			ConstString identifier;
			zero(&identifier);
			identifier.data = identifier_start;
			identifier.length = cursor - identifier_start;
			
			auto keyword_index_optional = find(Array<ConstString>(keyword_strings), identifier);
			if (keyword_index_optional) {
				auto new_token = push_new_token(TokenKind::keyword, identifier_start_site);
				new_token->keyword_value = (Keyword)keyword_index_optional.value;
			} else {
				auto new_token = push_new_token(TokenKind::identifier, identifier_start_site);
				new_token->identifier_value = identifier;
			}
		}
	}
	
	return tokens;
}

//---Linearizer types
struct SyntaxNode;
struct SyntaxNodeFunction;

enum class Type : u8 {
	integer,
	string
};

struct Value {
	SourceSite* declaration_site;
	ConstString identifier;
	Type type;
};

//---Parser
enum class SyntaxNodeKind : u8 {
	BinaryOperation,
	Identifier,
	IntegerLiteral,
	StringLiteral,
	Type,
	Function,
};

struct SyntaxNode {
	SourceSite* site;
	SyntaxNodeKind kind;
};

struct SyntaxNodeBinaryOperation : SyntaxNode {
	Keyword operator_keyword;
	SyntaxNode* operands[2];
};

struct SyntaxNodeIdentifier : SyntaxNode {
	ConstString identifier;
};

struct SyntaxNodeIntegerLiteral : SyntaxNode {
	u64 integer;
};

struct SyntaxNodeStringLiteral : SyntaxNode {
	ConstString string;
};

struct SyntaxNodeType : SyntaxNode {
	Type type;
};

struct SyntaxNodeFunction : SyntaxNode {
	ConstString name;
	Array<ConstString> argument_names;
	Array<SyntaxNode*> body;
	Array<SyntaxNode*> linear_nodes;
	Array<Value> declarations;
};

struct ParseContext {
	Token* cursor;
	Token* tokens_end;
};

internal Optional<Array<SyntaxNode*>> parse(Array<Token> tokens);
internal Optional<Array<SyntaxNode*>> parse_statements(ParseContext* parser);
internal SyntaxNodeFunction* parse_function(ParseContext* parser);
internal SyntaxNode* parse_expression(ParseContext* parser, u64 outer_precedence = 0);

template<typename AllocatorType> internal void write(String* buffer, Array<SyntaxNode*> syntax_nodes, fuint indent, AllocatorType* allocator);
template<typename AllocatorType> internal void write(String* buffer, SyntaxNode* syntax_node, fuint indent, AllocatorType* allocator);
template<typename AllocatorType> internal void write_function_opening(String* buffer, SyntaxNodeFunction* function, AllocatorType* allocator);
template<typename SyntaxNodeType> internal SyntaxNodeType* create_syntax_node_internal(SyntaxNodeKind kind, SourceSite* site);

template<typename SyntaxNodeType>
internal SyntaxNodeType* create_syntax_node_internal(SyntaxNodeKind kind, SourceSite* site) {
	auto syntax_node = allocate<SyntaxNodeType>(&heap_stack);
	syntax_node->kind = kind;
	syntax_node->site = site;
	return syntax_node;
}
#define create_syntax_node(type, site) create_syntax_node_internal<SyntaxNode##type>(SyntaxNodeKind::##type, site);


template<typename AllocatorType>
internal void write(String* buffer, Array<SyntaxNode*> syntax_nodes, fuint indent, AllocatorType* allocator) {
	fox_for (node_index, syntax_nodes.length) {
		write_indent(buffer, indent, allocator);
		write(buffer, syntax_nodes[node_index], indent, allocator);
		write(buffer, "\n", allocator);
	}
}

template<typename AllocatorType>
internal void write(String* buffer, SyntaxNode* syntax_node, fuint indent, AllocatorType* allocator) {
	switch (syntax_node->kind) {
		case SyntaxNodeKind::BinaryOperation: {
			auto binary_operation = (SyntaxNodeBinaryOperation*)syntax_node;
			write(buffer, "(", allocator);
			write(buffer, binary_operation->operands[0], indent, allocator);
			write(buffer, " ", allocator);
			write(buffer, binary_operation->operator_keyword, allocator);
			write(buffer, " ", allocator);
			write(buffer, binary_operation->operands[1], indent, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case SyntaxNodeKind::Identifier: {
			auto identifier = (SyntaxNodeIdentifier*)syntax_node;
			write(buffer, identifier->identifier, allocator);
		} break;
		
		case SyntaxNodeKind::IntegerLiteral: {
			auto integer = (SyntaxNodeIntegerLiteral*)syntax_node;
			write_uint(buffer, integer->integer, allocator);
		} break;
		
		case SyntaxNodeKind::StringLiteral: {
			//@todo escape string literals so we don't have line endings
			auto string = (SyntaxNodeStringLiteral*)syntax_node;
			write(buffer, "'", allocator);
			write(buffer, string->string, allocator);
			write(buffer, "'", allocator);
		} break;
		
		case SyntaxNodeKind::Function: {
			auto function = (SyntaxNodeFunction*)syntax_node;
			write_function_opening(buffer, function, &heap_stack);
			write(buffer, function->body, indent + 1, allocator);
			write(buffer, "}\n", allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

template<typename AllocatorType>
internal void write_function_opening(String* buffer, SyntaxNodeFunction* function, AllocatorType* allocator) {
	write(buffer, "function ", allocator);
	write(buffer, function->name, allocator);
	write(buffer, "(", allocator);
	fox_for (argument_index, function->argument_names.length) {
		write(buffer, function->argument_names[argument_index], allocator);
		write(buffer, ": int", allocator);
		if (argument_index != function->argument_names.length - 1) {
			write(buffer, ", ", allocator);
		}
	}
	write(buffer, "): int {\n", allocator);
}

internal bool expect_token_kind_internal(ParseContext* parser, TokenKind kind, ConstString where) {
	if (parser->cursor < parser->tokens_end) {
		if (parser->cursor->kind == kind) {
			return true;
		}
		
		String error_message;
		zero(&error_message);
		write(&error_message, "Syntax error: expected ", &heap_stack);
		
		ConstString kind_string;
		zero(&kind_string);
		switch (kind) {
			case TokenKind::keyword: {
				kind_string = "keyword";
			} break;
			
			case TokenKind::integer: {
				kind_string = "integer literal";
			} break;
			
			case TokenKind::string: {
				kind_string = "string literal";
			} break;
			
			case TokenKind::identifier: {
				kind_string = "identifier";
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
		
		write(&error_message, kind_string, &heap_stack);
		write(&error_message, " but got ", &heap_stack);
		write(&error_message, *parser->cursor, &heap_stack);
		write(&error_message, "\n", &heap_stack);
		print(error_message);
	} else {
		//@todo we should probably provide a different site for this
		print_unexpected_end_of_file(parser->cursor[-1].site, where);
	}
	
	return false;
}
#define expect_token_kind(parser, kind, where) if (!expect_token_kind_internal((parser), (kind), (where))) { return nullptr; }

internal bool expect_keyword_internal(ParseContext* parser, Keyword keyword, ConstString where) {
	if (expect_token_kind_internal(parser, TokenKind::keyword, where)) {
		if (parser->cursor->keyword_value == keyword) {
			parser->cursor++;
			return true;
		}
		
		String error_message;
		zero(&error_message);
		write(&error_message, parser->cursor->site, &heap_stack);
		write(&error_message, "Syntax error: expected keyword ", &heap_stack);
		write(&error_message, keyword, &heap_stack);
		write(&error_message, "\n", &heap_stack);
		print(error_message);
	}
	
	return false;
}
#define expect_keyword(parser, keyword, where) if (!expect_keyword_internal((parser), (keyword), (where))) { return nullptr; }

internal Optional<Array<SyntaxNode*>> parse(Array<Token> tokens) {
	fox_assert(tokens);
	
	Array<SyntaxNode*> syntax_tree;
	zero(&syntax_tree);
	
	ParseContext parser;
	zero(&parser);
	parser.cursor = tokens.data;
	parser.tokens_end = tokens.data + tokens.length;
	
	return parse_statements(&parser);
}

internal Optional<Array<SyntaxNode*>> parse_statements(ParseContext* parser) {
	Array<SyntaxNode*> statements;
	zero(&statements);
	
	while (parser->cursor < parser->tokens_end) {
		if (parser->cursor->kind == TokenKind::keyword) {
			switch (parser->cursor->keyword_value) {
				case Keyword::function: {
					auto function = parse_function(parser);
					if (function) {
						push(&statements, function, &heap_stack);
					} else {
						return nil;
					}
				} break;
				
				case Keyword::close_curly: {
					return statements;
				} break;
				
				default: {
					String error_message;
					zero(&error_message);
					write(&error_message, "Syntax error: unexpected keyword at top level ", &heap_stack);
					write(&error_message, parser->cursor->keyword_value, &heap_stack);
					write(&error_message, "\n", &heap_stack);
					print(error_message);
					return nil;
				} break;
			}
		} else {
			auto expression = parse_expression(parser);
			if (expression) {
				push(&statements, expression, &heap_stack);
			} else {
				return nil;
			}
		}
	}
	
	return statements;
}

internal SyntaxNodeFunction* parse_function(ParseContext* parser) {
	//function name(argument1: int, argument2: int,): int {}
	//function name() {}
	fox_assert(parser->cursor->kind == TokenKind::keyword && parser->cursor->keyword_value == Keyword::function);
	auto function_site = &parser->cursor->site;
	parser->cursor++;
	
	expect_token_kind(parser, TokenKind::identifier, "in function declaration, expected function name.\n");
	auto function_name = parser->cursor->identifier_value;
	parser->cursor++;
	
	expect_keyword(parser, Keyword::open_parenthesis, "in function declaration, expected open parenthesis.\n");
	
	//Parse arguments
	Array<ConstString> argument_names;
	zero(&argument_names);
	while (true) {
		if (parser->cursor >= parser->tokens_end) {
			print_unexpected_end_of_file(parser->cursor[-1].site, "in function declaration, expected close parenthesis or next argument.\n");
			return nullptr;
		}
		
		if (parser->cursor->kind == TokenKind::keyword) {
			expect_keyword(parser, Keyword::close_parenthesis, "in function declaration, expected close parenthesis or next argument.\n");
			break;
		}
		
		expect_token_kind(parser, TokenKind::identifier, "in function declaration, expected name of argument.\n");
		auto argument_name = parser->cursor->identifier_value;
		parser->cursor++;
		expect_keyword(parser, Keyword::declare, "in function declaration, expected type of argumnet.\n");
		expect_keyword(parser, Keyword::integer, "in function declaration, expected type of argumnet.\n");
		
		expect_token_kind(parser, TokenKind::keyword, "in function declaration, expected close parenthesis or next argument.\n");
		if (!(parser->cursor->keyword_value == Keyword::comma || parser->cursor->keyword_value == Keyword::close_parenthesis)) {
			String error_message;
			zero(&error_message);
			write(&error_message, "Syntax error: expected comma or close parenthesis but got ", &heap_stack);
			write(&error_message, *parser->cursor, &heap_stack);
			write(&error_message, "\n", &heap_stack);
			print(error_message);
			return nullptr;
		}
		
		if (parser->cursor->keyword_value == Keyword::comma) {
			parser->cursor++;
		}
	}
	
	expect_keyword(parser, Keyword::declare, "in function declaration, expected return value.\n");
	expect_keyword(parser, Keyword::integer, "in function declaration, expected a dumb stupid single integer return value????\n");
	
	expect_keyword(parser, Keyword::open_curly, "in function declaration, expected function body.\n");
	auto function_body = parse_statements(parser);
	if (function_body) {
		expect_keyword(parser, Keyword::close_curly, "in function declaration, expected end of function body.\n");
		
		auto function = create_syntax_node(Function, function_site);
		function->name = function_name;
		function->argument_names = argument_names;
		function->body = function_body.value;
		return function;
	}
	
	return nullptr;
}

internal SyntaxNode* parse_expression(ParseContext* parser, u64 outer_precedence) {
	fox_assert(parser->cursor < parser->tokens_end);
	
	SyntaxNode* left = nullptr;
	switch (parser->cursor->kind) {
		case TokenKind::identifier: {
			auto identifier = create_syntax_node(Identifier, &parser->cursor->site);
			identifier->identifier = parser->cursor->identifier_value;
			left = identifier;
		} break;
		
		case TokenKind::integer: {
			auto integer = create_syntax_node(IntegerLiteral, &parser->cursor->site);
			integer->integer = parser->cursor->integer_value;
			left = integer;
		} break;
		
		case TokenKind::string: {
			auto string = create_syntax_node(StringLiteral, &parser->cursor->site);
			string->string = parser->cursor->string_value;
			left = string;
		} break;
		
		case TokenKind::keyword: {
			auto keyword = parser->cursor->keyword_value;
			if (keyword == Keyword::integer) {
				auto type = create_syntax_node(Type, &parser->cursor->site);
				type->type = Type::integer;
			} else {
				print("Syntax error cant be keyword here");
				return nullptr;
			}
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
	
	parser->cursor++;
	while (parser->cursor < parser->tokens_end && parser->cursor->kind == TokenKind::keyword) {
		auto operator_keyword = parser->cursor->keyword_value;
		auto precedence = precedences[(fuint)operator_keyword];
		if (precedence > outer_precedence || (precedence == outer_precedence && get_associativity(operator_keyword) == Associativity::right)) {
			//Currently we only have binary operators
			auto binary_operation_site = &parser->cursor->site;
			auto binary_operator = create_syntax_node(BinaryOperation, binary_operation_site);
			binary_operator->operator_keyword = parser->cursor->keyword_value;
			binary_operator->operands[0] = left;
			
			parser->cursor++;
			if (parser->cursor < parser->tokens_end) {
				auto right = parse_expression(parser, precedence);
				if (right) {
					binary_operator->operands[1] = right;
					left = binary_operator;
				} else {
					return nullptr;
				}
			} else {
				print_unexpected_end_of_file(*binary_operation_site, "after binary operator.\n");
				return nullptr;
			}
		} else {
			break;
		}
	}
	
	return left;
}

//---Linearizer context
struct LinearizerContext {
	Array<SyntaxNodeFunction*> functions;
	Array<SyntaxNode*> global_statements;
	SyntaxNodeFunction global_function;
};

//---Linearizer
internal bool linearize_function(LinearizerContext* linearizer, SyntaxNodeFunction* function);
internal bool linearize_node(LinearizerContext* linearizer, SyntaxNode* node, SyntaxNodeFunction* containing_function);
internal void push_node(SyntaxNode* node, SyntaxNodeFunction* containing_function);
internal void print_statement_does_nothing_warning(SyntaxNode* statement);

internal bool linearize(LinearizerContext* linearizer, Array<SyntaxNode*> statements) {
	fox_for (statement_index, statements.length) {
		auto statement = statements[statement_index];
		switch (statement->kind) {
			case SyntaxNodeKind::BinaryOperation: {
				push(&linearizer->global_statements, statement, &heap_stack);
			} break;
			
			case SyntaxNodeKind::Identifier:
			case SyntaxNodeKind::IntegerLiteral:
			case SyntaxNodeKind::StringLiteral: {
				print_statement_does_nothing_warning(statement);
			} break;
			
			case SyntaxNodeKind::Function: {
				push(&linearizer->functions, (SyntaxNodeFunction*)statement, &heap_stack);
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	}
	
	fox_for (function_index, linearizer->functions.length) {
		if (!linearize_function(linearizer, linearizer->functions[function_index])) {
			return false;
		}
	}
	
	return linearize_function(linearizer, &linearizer->global_function);
}

internal bool linearize_function(LinearizerContext* linearizer, SyntaxNodeFunction* function) {
	fox_for (statement_index, function->body.length) {
		if (!linearize_node(linearizer, function->body[statement_index], function)) {
			return false;
		}
	}
	return true;
}

internal bool linearize_node(LinearizerContext* linearizer, SyntaxNode* node, SyntaxNodeFunction* containing_function) {
	switch (node->kind) {
		case SyntaxNodeKind::BinaryOperation: {
			auto binary_operation = (SyntaxNodeBinaryOperation*)node;
			if (binary_operation->operator_keyword == Keyword::declare) {
				auto declaration_identifier = (SyntaxNodeIdentifier*)binary_operation->operands[0];
				if (declaration_identifier->kind != SyntaxNodeKind::Identifier) {
					String error_message;
					zero(&error_message);
					write(&error_message, *declaration_identifier->site, &heap_stack);
					write(&error_message, "Syntax Error: expected identifier in declaration.\n", &heap_stack);
					print(error_message);
					return false;
				}
				
				fox_for (value_index, containing_function->declarations.length) {
					auto value = &containing_function->declarations[value_index];
					if (value->identifier == declaration_identifier->identifier) {
						String error_message;
						zero(&error_message);
						write(&error_message, *binary_operation->site, &heap_stack);
						write(&error_message, "Syntax Error: redeclaration of previously declared value.\n", &heap_stack);
						write(&error_message, *value->declaration_site, &heap_stack);
						write(&error_message, "Note: previous declaration is here.\n", &heap_stack);
						print(error_message);
						return false;
					}
				}
				
				auto type = (SyntaxNodeType*)binary_operation->operands[1];
				if (type->kind != SyntaxNodeKind::Type) {
					String error_message;
					zero(&error_message);
					write(&error_message, *type->site, &heap_stack);
					write(&error_message, "Syntax Error: expected type on right side of declaration.\n", &heap_stack);
					print(error_message);
					return false;
				}
				
				auto new_value = push_zero(&containing_function->declarations, &heap_stack);
				new_value->identifier = declaration_identifier->identifier;
				new_value->type = type->type;
			} else {
				if (!linearize_node(linearizer, binary_operation->operands[0], containing_function)
					|| !linearize_node(linearizer, binary_operation->operands[1], containing_function)) {
					return false;
				}
				push_node(binary_operation, containing_function);
			}
		} break;
		
		case SyntaxNodeKind::Identifier:
		case SyntaxNodeKind::IntegerLiteral:
		case SyntaxNodeKind::StringLiteral: {
			push_node(node, containing_function);
		} break;
		
		case SyntaxNodeKind::Function: {
			push(&linearizer->functions, (SyntaxNodeFunction*)node, &heap_stack);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
	
	return true;
}

internal void push_node(SyntaxNode* node, SyntaxNodeFunction* containing_function) {
	push(&containing_function->linear_nodes, node, &heap_stack);
}

internal void print_statement_does_nothing_warning(SyntaxNode* statement) {
	auto restore_point = create_restore_point(&heap_stack);
	String error_message;
	zero(&error_message);
	write(&error_message, *statement->site, &heap_stack);
	write(&error_message, "Warning: statement does nothing.", &heap_stack);
	print(error_message);
	restore(&heap_stack, restore_point);
}

template<typename AllocatorType>
internal void write(String* buffer, LinearizerContext* linearizer, AllocatorType* allocator) {
	write(buffer, "Functions:\n\n", allocator);
	fox_for (function_index, linearizer->functions.length) {
		auto function = linearizer->functions[function_index];
		write_function_opening(buffer, function, allocator);
		write(buffer, function->linear_nodes, 1, allocator);
		write(buffer, "}\n\n", allocator);
	}
	
	write(buffer, "Global statements:\n", allocator);
	write(buffer, linearizer->global_function.linear_nodes, 1, allocator);
}

//---Interpreter
internal void interpret(String file_string, ConstString file_path) {
	auto maybe_tokens = lex(file_string, file_path);
	fox_assert(maybe_tokens);
	auto tokens = maybe_tokens.value;
	
#if enable_lexer_print
	String tokenized_file;
	zero(&tokenized_file);
	fox_for (itoken, tokens.length) {
		write(&tokenized_file, tokens[itoken], &heap_stack);
		write(&tokenized_file, "\n", &heap_stack);
		write(&tokenized_file, tokens[itoken].site, &heap_stack);
		write(&tokenized_file, "\n", &heap_stack);
	}
	print(tokenized_file);
#endif
	
	auto maybe_syntax_tree = parse(tokens);
	fox_assert(maybe_syntax_tree);
	auto syntax_tree = maybe_syntax_tree.value;
	
#if enable_parser_print
	String syntax_tree_string;
	zero(&syntax_tree_string);
	write(&syntax_tree_string, syntax_tree, 0, &heap_stack);
	print(syntax_tree_string);
#endif
	
	LinearizerContext linearizer;
	zero(&linearizer);
	auto linearize_successful = linearize(&linearizer, syntax_tree);
	fox_assert(linearize_successful);
	
#if enable_linearizer_print
	String linearizer_string;
	zero(&linearizer_string);
	write(&linearizer_string, &linearizer, &heap_stack);
	print(linearizer_string);
#endif
}

int main(int argument_count, char** arguments) {
	test_foxlib();
	
	u64 memory_size = mebibytes(1);
	initialize(&heap_stack, calloc(memory_size, 1), memory_size);
	
	if (argument_count > 1) {
		ConstString file_path = arguments[1];
		Array<u8> file_data;
		zero(&file_data);
		auto file_read_status = read_entire_file(file_path.data, &heap_stack, &file_data);
		switch (file_read_status) {
			case FileStatus::read: {
				auto file_string = fox_interpret_cast(String, file_data);
				interpret(file_string, file_path);
			} break;
			
			case FileStatus::empty: {
				String error_message;
				zero(&error_message);
				write(&error_message, "File ", &heap_stack);
				write(&error_message, file_path, &heap_stack);
				write(&error_message, " is empty!\n", &heap_stack);
				print(error_message);
			} break;
			
			case FileStatus::error: {
				String error_message;
				zero(&error_message);
				write(&error_message, "File ", &heap_stack);
				write(&error_message, file_path, &heap_stack);
				write(&error_message, " is inaccessible!\n", &heap_stack);
				print(error_message);
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	} else {
		print("Usage: vulpes.exe <file to compile>\n");
	}
	
	return 0;
}