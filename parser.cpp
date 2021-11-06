#include <list>
#include <cstdarg>
//structures Expr and Stmt made with a lot of help from LJMSU
//cause idk and not really want to know much about oop lol

struct Expr {
  virtual ~Expr() {}

  struct Assign;
  struct Binary;
  struct Call;
  struct Grouping;
  struct Literal;
  struct Logical;
  struct Unary;
  struct Variable;

  template <typename R>
  struct Visitor {
    virtual ~Visitor();

    R visitAssignExpr(Assign expr);
    R visitBinaryExpr(Binary expr);
    R visitCallExpr(Call expr);
    R visitGroupingExpr(Grouping expr);
    R visitLiteralExpr(Literal expr);
    R visitLogicalExpr(Logical expr);
    R visitUnaryExpr(Unary expr);
    R visitVariableExpr(Variable expr);
  };

  template <typename R>
  R accept(Visitor<R> visitor);
};

struct Expr::Assign : Expr {
  const token name;
  const Expr value;
  Assign(token name, Expr value)
      : name(name), value(value){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitAssignExpr(this);
  };
};

struct Expr::Binary : Expr {
  const Expr left;
  const token op;
  const Expr right;
  Binary(Expr left, token op, Expr right)
      : left(left), op(op), right(right){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitBinaryExpr(this);
  };
};

struct Expr::Call : Expr {
  const Expr callee;
  const token paren;
  const std::vector<Expr> arguments;
  Call(Expr callee, token paren, std::vector<Expr> arguments)
      : callee(callee), paren(paren), arguments(arguments){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitCallExpr(this);
  };
};

struct Expr::Grouping : Expr
{
  const Expr expression;
  Grouping(Expr expression)
      : expression(expression){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitGroupingExpr(this);
  };
};

struct Expr::Literal : Expr
{
  const std::string value;
  Literal(std::string value)
      : value(value){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitLiteralExpr(this);
  };
};

struct Expr::Logical : Expr
{
  const Expr left;
  const token op;
  const Expr right;
  Logical(Expr left, token op, Expr right)
      : left(left), op(op), right(right){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitLogicalExpr(this);
  };
};

struct Expr::Unary : Expr
{
  const token op;
  const Expr right;
  Unary(token op, Expr right)
      : op(op), right(right){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitUnaryExpr(this);
  };
};

struct Expr::Variable : Expr
{
  const token name;
  Variable(token name)
      : name(name){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitVariableExpr(this);
  };
};

struct Stmt
{
  ~Stmt();

  struct Block;
  struct Expression;
  struct Function;
  struct If;
  struct Return;
  struct Local;
  struct While;

  template <typename R>
  struct Visitor
  {
    ~Visitor();

    R visitBlockStmt(Block stmt);
    R visitExpressionStmt(Expression stmt);
    R visitFunctionStmt(Function stmt);
    R visitIfStmt(If stmt);
    R visitReturnStmt(Return stmt);
    R visitLocalStmt(Local stmt);
    R visitWhileStmt(While stmt);
  };

  template <typename R>
  R accept(Visitor<R> visitor);
};

struct Stmt::Block : Stmt
{
  const std::list<Stmt> statements;
  Block(std::list<Stmt> statements)
      : statements(statements){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitBlockStmt(this);
  };
};

struct Stmt::Expression : Stmt
{
  const Expr expression;
  Expression(Expr expression)
      : expression(expression){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitExpressionStmt(this);
  };
};

struct Stmt::Function : Stmt
{
  const token name;
  const std::list<token> params;
  const std::list<Stmt> body;
  Function(token name, std::list<token> params, std::list<Stmt> body)
      : name(name), params(params), body(body){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitFunctionStmt(this);
  };
};

struct Stmt::If : Stmt
{
  const Expr condition;
  const Stmt thenBranch;
  const Stmt elseBranch;
  If(Expr condition, Stmt thenBranch, Stmt elseBranch)
      : condition(condition), thenBranch(thenBranch), elseBranch(elseBranch){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitIfStmt(this);
  };
};

struct Stmt::Return : Stmt
{
  const token keyword;
  const Expr value;
  Return(token keyword, Expr value)
      : keyword(keyword), value(value){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitReturnStmt(this);
  };
};

struct Stmt::Local : Stmt
{
  const token name;
  const Expr initializer;
  Local(token name, Expr initializer)
      : name(name), initializer(initializer){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitLocalStmt(this);
  };
};

struct Stmt::While : Stmt
{
  const Expr condition;
  const Stmt body;
  While(Expr condition, Stmt body)
      : condition(condition), body(body){};

  template <typename R>
  R accept(Visitor<R> visitor)

  {
    return visitor.visitWhileStmt(this);
  };
};

struct parserinfo {
	uint current = 0;
	std::vector<token> tokens;
	
	parserinfo(std::vector<token> tokens) :
		tokens(tokens)
	{}
	
	Expr expression() {
		return equality();
	}
	
	Expr equality() {
		Expr expr = comparison();
		while (match(2, NOT_EQUAL, EQUAL)) {
			token op = previous();
			Expr right = comparison();
			expr = Expr::Binary(expr, op, right);
		}
		return expr;
	}
	
	bool match(uint count, ...) {
		std::va_list args;
		va_start(args, count);
		for (uint i = 0; i < count; i++) {
			tokentype type = va_arg(args, tokentype);
			if (check(type)) {
				advance();
				va_end(args);
				return true;
			}
		}
		va_end(args);
		return false;
	}
	
	bool check(tokentype type) {
		if (ended()) return false;
		return peek().type == type;
	}
	
	token advance() {
		if (!ended()) current++;
		return previous();
	}
	
	bool ended() {
		return peek().type == EOF;
	}
	
	token peek() {
		return tokens.at(current);
	}
	
	token previous() {
		return tokens.at(current - 1);
	}
	
	Expr comparison() {
		Expr expr = term();
		while(match(4, BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL)) {
			token op = previous();
			Expr right = term();
			expr = Expr::Binary(expr, op, right);
		}
		return expr;
	}
	
	Expr term() {
		Expr expr = factor();
		while (match(2, MINUS, PLUS)) {
			token op = previous();
			Expr right = factor();
			expr = Expr::Binary(expr, op, right);
		}
		return expr;
	}
	
	Expr factor() {
		Expr expr = unary();
		while (match(2, SLASH, STAR)) {
			token op = previous();
			Expr right = unary();
			expr = Expr::Binary(expr, op, right);
		}
		return expr;
	}
	
	Expr unary() {
		if (match(2, NOT, MINUS)) {
			token op = previous();
			Expr right = unary();
			return Expr::Unary(op, right);
		}
		return primary();
	}
	
	Expr primary() {
		if (match(1, FALSE)) return Expr::Literal("false");
		if (match(1, TRUE)) return Expr::Literal("true");
		if (match(1, NIL)) return Expr::Literal("nil");
		if (match(2, NUMBER, STRING)) {
			return Expr::Literal(previous().literal);
		}
		if (match(1, ROUND_BRACKET_OPEN)) {
			Expr expr = expression();
			consume(ROUND_BRACKET_CLOSED, "Expect ')' after expression.");
			return Expr::Grouping(expr);
		}
	}
	
	token consume(tokentype type, )
};

void parsetokens(std::vector<token> tokens, std::string filename) {
	
}
