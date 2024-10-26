import { Import, Type, ParsedCodeFile } from '../../common/model/code-representation';

/**
 * This is the interface that all language handlers must implement. It defines the methods that the code parser will
 * use to parse the code for that language.
 */
export interface LanguageHandler {
  /**
   * Parses the provided code and returns the imports.
   *
   * @param code The code to parse
   */
  parseImports(code: string): Import[];

  /**
   * Parses the provided code and returns the classes.
   *
   * @param code The code to parse
   */
  parseTypes(code: string): Type[];
}

export class CodeParser {
  constructor(private readonly languageHandler: LanguageHandler) {}

  /**
   * Parses the provided code.
   * @param code The code to parse. This is expected to be the entire code of a file.
   */
  parseCode(code: string): ParsedCodeFile {
    // get the imports
    const imports: Import[] = this.languageHandler.parseImports(code);
    // get the classes
    const types: Type[] = this.languageHandler.parseTypes(code);

    return {
      imports,
      types,
    };
  }
}
