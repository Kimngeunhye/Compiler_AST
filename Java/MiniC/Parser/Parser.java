package MiniC.Parser;

import MiniC.Scanner.Scanner;
import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.ErrorReporter;
import MiniC.AstGen.*;


public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;
  private SourcePos previousTokenPosition;

  // Constructor of the Parser class. Receives the scanner and ErrorReporter
  // as arguments.
  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  private void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      previousTokenPosition = currentToken.GetSourcePos();
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  private void acceptIt() {
    previousTokenPosition = currentToken.GetSourcePos();
    currentToken = scanner.scan();
  }

  // start records the position of the start of a phrase.
  // This is defined to be the position of the first
  // character of the first token of the phrase.
  private void start(SourcePos pos) {
    pos.StartCol = currentToken.GetSourcePos().StartCol;
    pos.StartLine = currentToken.GetSourcePos().StartLine;
  }

  // finish records the position of the end of a phrase.
  // This is defined to be the position of the last
  // character of the last token of the phrase.
  private void finish(SourcePos pos) {
    pos.EndCol = previousTokenPosition.EndCol;
    pos.EndLine = previousTokenPosition.EndLine;
  }

  private void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  private boolean isTypeSpecifier(int token) {
    if(token == Token.VOID ||
            token == Token.INT  ||
            token == Token.BOOL ||
            token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArrayIndexDecl (Type T):
  //
  // Take [INTLITERAL] and generate an ArrayType
  //
  ///////////////////////////////////////////////////////////////////////////////

  private ArrayType parseArrayIndexDecl(Type T) throws SyntaxError {
    IntLiteral L;
    IntExpr IE;
    accept(Token.LEFTBRACKET);
    SourcePos pos = currentToken.GetSourcePos();
    L = new IntLiteral(currentToken.GetLexeme(), pos);
    accept(Token.INTLITERAL);
    accept(Token.RIGHTBRACKET);
    IE = new IntExpr (L, pos);
    return new ArrayType (T, IE, previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Program parse() { // called from the MiniC driver

    Program ProgramAST = null;

    previousTokenPosition = new SourcePos();
    previousTokenPosition.StartLine = 0;
    previousTokenPosition.StartCol = 0;
    previousTokenPosition.EndLine = 0;
    previousTokenPosition.EndCol = 0;

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      ProgramAST = parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
                currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) { return null; }
    return ProgramAST;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  // parseProgDecls: recursive helper function to facilitate AST construction.
  private Decl parseProgDecls () throws SyntaxError {
    if (! isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    SourcePos pos = new SourcePos();
    start(pos);
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    if(currentToken.kind == Token.LEFTPAREN) {
      Decl newD = parseFunPart(T, Ident, pos);
      return new DeclSequence (newD, parseProgDecls(), previousTokenPosition);
    } else {
      DeclSequence Vars = parseVarPart(T, Ident);
      DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
      Decl RemainderDecls = parseProgDecls();
      VarsTail.SetRightSubtree (RemainderDecls);
      return Vars;
    }
  }

  private Program parseProgram() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    Decl D = parseProgDecls();
    finish(pos);
    Program P = new Program (D, pos);
    return P;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Decl parseFunPart(Type T, ID Ident, SourcePos pos) throws SyntaxError {

    // We already know that the current token is "(".
    // Otherwise use accept() !
    acceptIt(); //"("
    Decl PDecl = parseParamsList(); // can also be empty...
    accept(Token.RIGHTPAREN);
    CompoundStmt CStmt = parseCompoundStmt();
    finish(pos);
    return new FunDecl (T, Ident, PDecl, CStmt, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParameterDecl ( "," ParameterDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Decl parseParamsList() throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyFormalParamDecl(previousTokenPosition);
    }
    Decl Decl_1 = parseParameterDecl();
    Decl Decl_r = new EmptyFormalParamDecl(previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
      Decl_r = parseParamsList();
      if (Decl_r instanceof EmptyFormalParamDecl) {
        syntaxError("Declaration after comma expected", "");
      }
    }
    return new FormalParamDeclSequence (Decl_1, Decl_r, previousTokenPosition);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParameterDecl():
  //
  // ParameterDecl ::= (VOID|INT|BOOL|FLOAT) Declarator
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Decl parseParameterDecl() throws SyntaxError {
    Type T = null;
    Decl D = null;

    SourcePos pos = new SourcePos();
    start(pos);
    if (isTypeSpecifier(currentToken.kind)) {
      T = parseTypeSpecifier();
    } else {
      syntaxError("Type specifier instead of % expected",
              Token.spell(currentToken.kind));
    }
    D = parseDeclarator(T, pos);
    return D;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseDeclarator():
  //
  // Declarator ::= ID ( "[" INTLITERAL "]" )?
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Decl parseDeclarator(Type T, SourcePos pos) throws SyntaxError {
    ID Ident = parseID();
    if (currentToken.kind == Token.LEFTBRACKET) {
      ArrayType ArrT = parseArrayIndexDecl(T);
      finish(pos);
      return new FormalParamDecl (ArrT, Ident, pos);
    }
    finish(pos);
    return new FormalParamDecl (T, Ident, pos);
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseVarPart():
  //
  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  //
  ///////////////////////////////////////////////////////////////////////////////

  private DeclSequence parseVarPart(Type T, ID Ident) throws SyntaxError {
    Type theType = T;
    DeclSequence Seq = null;
    if (currentToken.kind == Token.LEFTBRACKET) {
      theType = parseArrayIndexDecl(T);
    }

    Expr E = new EmptyExpr(previousTokenPosition);
    if (currentToken.kind == Token.ASSIGN) {
      acceptIt();
      // You can use the following code after you have implemented
      // parseInitializer():
      E = parseInitializer(theType);
    }

    Decl D;
    D = new VarDecl (theType, Ident, E, previousTokenPosition);
    // You can use the following code after implementatin of parseInitDecl():
       if (currentToken.kind == Token.COMMA) {
       acceptIt();
       Seq = new DeclSequence (D, parseInitDecl(T), previousTokenPosition);
       } else {
       Seq = new DeclSequence (D, new EmptyDecl (previousTokenPosition), previousTokenPosition);
       }
    accept (Token.SEMICOLON); //var1:35
    return Seq; //var10
  }
  /*
   * initializer ::= expr | "{" expr initializer-tail "}"
     initializer-tail ::= ("," expr)*
   * */
  private Expr parseInitializer(Type t) throws SyntaxError{
    if(currentToken.kind == Token.LEFTBRACE) //"{"
    {
      acceptIt();
      Expr expr = parseExpr();
      ExprSequence exprSequence = parseInitializerTail(t);
      accept(Token.RIGHTBRACE); //"}"
      SourcePos pos = null;
      return new EmptyExpr(pos);

    }
    else 
      return parseExpr();
  }

  private Expr parseExpr() throws SyntaxError{
    return parsePrimaryExpr();
  }

  private ExprSequence parseInitializerTail(Type t) throws SyntaxError{
    SourcePos pos = null;
    if(currentToken.kind == Token.COMMA)
    {
      acceptIt();
      Expr expr = parseExpr();
      ExprSequence exprSequence = parseInitializerTail(t);
      return new ExprSequence(expr, exprSequence, previousTokenPosition);
    }
    else {
      //COMMA가 아닌 경우, ExprSequence를 반환
      Expr lAST = parseExpr();
      Expr rAST = parseInitializerTail(t);
      return new ExprSequence(lAST, rAST, currentToken.GetSourcePos());
    }
  }

  //InitDecl() ::= declarator ("=" initializer initDeclTail)?
  private Decl parseInitDecl(Type t) throws SyntaxError{
    Decl declarator = parseDeclarator(t, previousTokenPosition);
    if(currentToken.kind == Token.ASSIGN)
    {
      acceptIt();
      Expr initializer = parseInitializer(t);
      Decl initDeclTail = parseInitDeclTail(t);
      SourcePos pos = null;
      return new EmptyDecl(pos);
    }
    return declarator;
  }

  //InitDeclTail ::= ( "," declarator ("=" initializer)? )*
  private Decl parseInitDeclTail(Type t) throws SyntaxError{
    if(currentToken.kind == Token.COMMA)
    {
      acceptIt();
      Decl declarator = parseDeclarator(t, previousTokenPosition);
      if(currentToken.kind == Token.ASSIGN)
      {
        acceptIt();
        Expr initializer = parseInitializer(t);
        Decl initDeclTail = parseInitDecl(t);
        SourcePos pos = null;
        return new EmptyDecl(pos);
      }
      else {
        SourcePos pos = null;
        return new EmptyDecl(pos);
      }
    }
    else
      return new EmptyDecl(previousTokenPosition);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseUnaryExpr():
  //
  // UnaryExpr ::= ("+"|"-"|"!")* PrimaryExpr
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Expr parseUnaryExpr() throws SyntaxError {
    if (currentToken.kind == Token.PLUS ||
            currentToken.kind == Token.MINUS ||
            currentToken.kind == Token.NOT) {
      Operator opAST = new Operator (currentToken.GetLexeme(),
              previousTokenPosition);
      acceptIt();
      return new UnaryExpr (opAST, parseUnaryExpr(), previousTokenPosition);
    }
    return parsePrimaryExpr();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parsePrimaryExpr():
  //
  // PrimaryExpr ::= ID arglist?
  //              |  ID "[" expr "]"
  //              |  "(" expr ")"
  //              |  INTLITERAL | BOOLLITERAL | FLOATLITERAL | STRINGLITERAL
  //
  ///////////////////////////////////////////////////////////////////////////////

  public <Lliteral> Expr parsePrimaryExpr() throws SyntaxError {
    Expr retExpr = null;

    SourcePos pos = new SourcePos();
    start(pos);

    if(currentToken.kind == Token.ID)
    {
      ID id = parseID();

      if(currentToken.kind == Token.LEFTPAREN)
      {
        //ID arglist?
        Expr argList = parseArgs();
        accept(Token.RIGHTPAREN);
        finish(pos);
        retExpr = new CallExpr(id, argList, pos);
      }
      else if(currentToken.kind == Token.LEFTBRACKET)
      {
        //ID "[" expr "]"
        acceptIt();
        Expr indexExpr = parseExpr();
        accept(Token.RIGHTBRACKET);
        finish(pos);
        retExpr = new ArrayExpr(retExpr, indexExpr, pos);
      }
      else
      {
        finish(pos);
        retExpr = new VarExpr(id, pos);
      }
    }
    else if(currentToken.kind == Token.LEFTPAREN)
    {
      //"(" expr ")"
      acceptIt();
      Expr expr = parseExpr();
      accept(Token.RIGHTPAREN);
      finish(pos);
      retExpr = expr;
    }
    else if(currentToken.kind == Token.INTLITERAL ||
      currentToken.kind == Token.BOOLLITERAL ||
      currentToken.kind == Token.FLOATLITERAL ||
      currentToken.kind == Token.STRINGLITERAL)
    {
      Lliteral literal = parseLiteral();
      finish(pos);
      retExpr = (Expr) literal;
    }
    else {
      syntaxError("Invalid expression", "");
    }
    return retExpr;
  }

  private <Lliteral> Lliteral parseLiteral() throws SyntaxError{
    Lliteral literal = null;
    SourcePos pos = currentToken.GetSourcePos();

    switch (currentToken.kind)
    {
      case Token.INTLITERAL:
        literal = (Lliteral) new IntLiteral(currentToken.GetLexeme(), pos);
        acceptIt();
        break;
      case Token.BOOLLITERAL:
        literal = (Lliteral) new BoolLiteral(currentToken.GetLexeme(), pos);
        acceptIt();
        break;
      case Token.FLOATLITERAL:
        literal = (Lliteral) new FloatLiteral(currentToken.GetLexeme(), pos);
        acceptIt();
        break;
      case Token.STRINGLITERAL:
        literal = (Lliteral) new StringLiteral(currentToken.GetLexeme(), pos);
        acceptIt();
        break;
      default:
        syntaxError("Invalid literal", "");
    }
    return literal;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDef* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Decl parseCompoundDecls () throws SyntaxError {
    if (!isTypeSpecifier(currentToken.kind)) {
      return new EmptyDecl (previousTokenPosition);
    }
    Type T = parseTypeSpecifier();
    ID Ident = parseID();
    DeclSequence Vars = parseVarPart(T, Ident);
    DeclSequence VarsTail = Vars.GetRightmostDeclSequenceNode();
    Decl RemainderDecls = parseCompoundDecls();
    VarsTail.SetRightSubtree (RemainderDecls);
    return Vars;
  }

  private Stmt parseCompoundStmts () throws SyntaxError {
    if (! (currentToken.kind == Token.LEFTBRACE ||
            currentToken.kind == Token.IF ||
            currentToken.kind == Token.WHILE ||
            currentToken.kind == Token.FOR ||
            currentToken.kind == Token.RETURN ||
            currentToken.kind == Token.ID)
    ) {
      return new EmptyStmt(previousTokenPosition);
    }
    Stmt S = null;
    // You can use the following code after implementation of parseStmt():
    //S = parseStmt();
    return new StmtSequence (S, parseCompoundStmts(), previousTokenPosition);
  }

  private CompoundStmt parseCompoundStmt() throws SyntaxError {
    SourcePos pos = new SourcePos();
    start(pos);
    accept(Token.LEFTBRACE);
    Decl D = parseCompoundDecls();
    Stmt S = parseCompoundStmts();
    accept(Token.RIGHTBRACE);
    finish(pos);
    if ( (D.getClass() == EmptyDecl.class) &&
            (S.getClass() == EmptyStmt.class)) {
      return new EmptyCompoundStmt (previousTokenPosition);
    } else {
      return new CompoundStmt (D, S, pos);
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseArgList():
  //
  // ArgList ::= "(" ( arg ( "," arg )* )? ")"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public Expr parseArgList() throws SyntaxError {
    accept(Token.LEFTPAREN);
    Expr Params = parseArgs();
    accept(Token.RIGHTPAREN);
    return Params;
  }

  //Args ::= ( arg ( "," arg )* )?
  private Expr parseArgs() throws SyntaxError {
    if (currentToken.kind == Token.RIGHTPAREN) {
      return new  EmptyActualParam (previousTokenPosition);
    }
    Expr Params = null;
    Params = new ActualParam (parseExpr(), previousTokenPosition);
    if (currentToken.kind == Token.COMMA) {
      acceptIt();
    }
    return new ActualParamSequence (Params, parseArgs(), previousTokenPosition);
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseID():
  //
  // ID (terminal)
  //
  ///////////////////////////////////////////////////////////////////////////////

  private ID parseID() throws SyntaxError {
    ID Ident = new ID(currentToken.GetLexeme(), currentToken.GetSourcePos());
    accept(Token.ID);
    return Ident;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseTypeSpecifier():
  //
  // VOID | INT | FLOAT | BOOL (all terminals)
  //
  ///////////////////////////////////////////////////////////////////////////////

  private Type parseTypeSpecifier() throws SyntaxError {
    Type T = null;
    switch (currentToken.kind) {
      case Token.INT:
        T = new IntType(currentToken.GetSourcePos());
        break;
      case Token.FLOAT:
        T = new FloatType(currentToken.GetSourcePos());
        break;
      case Token.BOOL:
        T = new BoolType(currentToken.GetSourcePos());
        break;
      case Token.VOID:
        T = new VoidType(currentToken.GetSourcePos());
        break;
      default:
        syntaxError("Type specifier expected", "");
    }
    acceptIt();
    return T;
  }

}
