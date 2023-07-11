package MiniC.Scanner;

import java.io.PrintStream;

public class Scanner {
  private SourceFile sourceFile;
  private boolean verbose;
  private char currentChar;
  private StringBuffer currentLexeme;
  private boolean currentlyScanningToken;
  private int currentLineNr;
  private int currentColNr;
  private int StartLine;
  private int EndLine;
  private int StartCol;
  private int EndCol;


  private boolean isDigit(char c) {
    return (c >= '0' && c <= '9');
  }

  private boolean isLetter(char c) {
    return ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z'));
  }

  public Scanner(SourceFile source) {
    sourceFile = source;
    currentChar = sourceFile.readChar();
    verbose = false;
    currentLineNr = 1;
    currentColNr = 1;
  }

  public void enableDebugging() {
    verbose = true;
  }

  private void takeIt() {
    if (currentlyScanningToken) {
      currentLexeme.append(currentChar);
    }
    if (currentChar == '\n') {
      currentLineNr++;
      currentColNr = 1;
    } else {
      if (currentColNr == 1) {
        StartCol = currentColNr;
      }
      EndCol = currentColNr;
      currentColNr++;
      if (currentChar == SourceFile.EOF) {
        EndCol--;
      }
    }
    currentChar = sourceFile.readChar();
  }


  private int scanToken() {
    switch (currentChar) {
      case '0':
      case '1':
      case '2':
      case '3':
      case '4':
      case '5':
      case '6':
      case '7':
      case '8':
      case '9':
        takeIt();
        while (isDigit(currentChar)) {
          takeIt();
        }
        if (currentChar == '.' || currentChar == 'E' || currentChar == 'e') {
          if (currentChar == '.') {
            takeIt();
            if (!isDigit(currentChar)) {
              return Token.FLOATLITERAL;
            }
            while (isDigit(currentChar))
              takeIt();
          }
          if (currentChar == 'E' || currentChar == 'e') {
            takeIt();
            if (currentChar == '+' || currentChar == '-') {
              takeIt();
            }
            if (!isDigit(currentChar)) {
              return Token.ERROR;
            }
            while (isDigit(currentChar)) {
              takeIt();
            }
          }
          return Token.FLOATLITERAL;
        } else {
          return Token.INTLITERAL;
        }
      case '.':
        takeIt();
        while (isDigit(currentChar)) {
          takeIt();
          if (currentChar == 'E' || currentChar == 'e') {
            takeIt();
            if (!isDigit(currentChar))
              return Token.ERROR;
            while (isDigit(currentChar))
              takeIt();
          }
          return Token.FLOATLITERAL;
        }
      case '_':
        takeIt();
        while (currentChar == '_') {
          takeIt();
          if (currentChar != '_')
            break;
        }
        return Token.ID;
      case '+':
        takeIt();
        return Token.PLUS;
      case '-':
        takeIt();
        return Token.MINUS;
      case '*':
        takeIt();
        return Token.TIMES;
      case '{':
        takeIt();
        return Token.LEFTBRACE;
      case '}':
        takeIt();
        return Token.RIGHTBRACE;
      case '(':
        takeIt();
        return Token.LEFTPAREN;
      case ')':
        takeIt();
        return Token.RIGHTPAREN;
      case '[':
        takeIt();
        ;
        return Token.LEFTBRACKET;
      case ']':
        takeIt();
        return Token.RIGHTBRACKET;
      case ',':
        takeIt();
        return Token.COMMA;
      case ';':
        takeIt();
        return Token.SEMICOLON;
      case '>':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.GREATEREQ;
        } else {
          return Token.GREATER;
        }
      case '<':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.LESSEQ;
        } else {
          return Token.LESS;
        }
      case '"':
        currentlyScanningToken = false;
        takeIt();
        currentlyScanningToken = true;

        while(currentChar != '\n')
        {
          switch(currentChar)
          {
            case '"':
              currentlyScanningToken = false;
              takeIt();
              currentlyScanningToken = true;
              return Token.STRINGLITERAL;
            case '\\':
              takeIt();
              if(currentChar != 'n') {
                isLetter('c');
              }
              takeIt();
              break;
            default:
              takeIt();
          }
        }
        isLetter('c');
        return Token.STRINGLITERAL;
      case '|':
        takeIt();
        if (currentChar == '|') {
          takeIt();
          return Token.OR;
        } else {
          takeIt();
          return Token.ERROR;
        }
      case '&':
        takeIt();
        if (currentChar == '&') {
          takeIt();
          return Token.AND;
        } else {
          takeIt();
          return Token.ERROR;
        }
      case '=':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.EQ;
        } else {
          return Token.ASSIGN;
        }
      case '!':
        takeIt();
        if (currentChar == '=') {
          takeIt();
          return Token.NOTEQ;
        } else {
          takeIt();
          return Token.NOT;
        }
      case SourceFile.EOF:
        currentLexeme.append('$');
        return Token.EOF;
      default:
        if (isLetter(currentChar)) {
          takeIt();
          while (isLetter(currentChar) || isDigit(currentChar)) {
            takeIt();
          }
          String lexeme = currentLexeme.toString();
          switch (lexeme) {  //예약어 그리고 ID
            case "else":
              return Token.ELSE;
            case "if":
              return Token.IF;
            case "int":
              return Token.INT;
            case "return":
              return Token.RETURN;
            case "void":
              return Token.VOID;
            case "while":
              return Token.WHILE;
            case "for":
              return Token.FOR;
            case "float":
              return Token.FLOAT;
            case "bool":
              return Token.BOOL;
            case "true":
              return Token.BOOLLITERAL;
            case "false":
              return Token.BOOLLITERAL;
            default:
              return Token.ID;
          }
        }
        takeIt();
        return Token.ERROR;
    }
  }

  public Token scan(){
    Token currentToken;
    SourcePos pos;
    int kind;

    currentlyScanningToken = false;

    while (currentChar == ' '
            || currentChar == '\f'
            || currentChar == '\n'
            || currentChar == '\r'
            || currentChar == '\t'){
      takeIt();
    }

    switch (currentChar)
    {
      case '/':
        takeIt();
        if(currentChar == '/')
        {
          while(currentChar != '\n')
            takeIt();
        }
        else if(currentChar == '*')
        {
          takeIt();
          boolean commentClosed = false;
          while(true)
          {
            if(currentChar == SourceFile.EOF) {
              commentClosed = false;
              break;
            }
            if(currentChar == '*') {
              takeIt();
              if(currentChar == '/') {
                commentClosed = true;
                takeIt();
                break;
              }
            } else {
              takeIt();
            }
          }
        }
    }

    currentlyScanningToken = true;
    currentLexeme = new StringBuffer("");
    pos = new SourcePos();
    pos.StartLine = currentLineNr;
    pos.EndLine = currentLineNr;
    pos.StartCol = currentColNr;

    kind = scanToken();
    currentToken = new Token(kind, currentLexeme.toString(), pos);

    if(currentChar == ' ' || currentChar == '\f' || currentChar == '\n' || currentChar == '\r' || currentChar == '\t')
      pos.EndCol = currentColNr - 1;
    else
      pos.EndCol = currentColNr;

    if (verbose)
      currentToken.print();

    return currentToken;
  }
};