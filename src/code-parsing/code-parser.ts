import * as core from '@actions/core';
import * as fs from 'fs';
import * as path from 'path';

import { Import, Type } from '../common/model/code-representation';

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

/**
 * Class that walks through a directory and parses all the code files in it.
 */
export class CodeBaseParser {
  /**
   * Parses the provided code.
   * @param code The code to parse. This is expected to be the entire code of a file.
   */
  readDirectory(directory: string, level?: number): void {
    const directoryContents = fs.readdirSync(directory);

    for (const file of directoryContents) {
      const filePath = path.join(directory, file);
      const stats = fs.statSync(filePath);

      if (stats.isDirectory()) {
        core.debug(`Directory: ${filePath}`);
        // Recursively read the directory
        this.readDirectory(filePath, (level ?? 0) + 1);
      } else if (stats.isFile()) {
        core.info(`File: ${file}`);
        const fileParser = new CodeFileParser();
        fileParser.parse(filePath);
      }
    }
  }
}

/**
 * Class that parses a code file.
 */
export class CodeFileParser {
  /**
   * Parses the provided code file.
   *
   * @param codeFilePath The path of the code file to parse.
   */
  parse(codeFilePath: string): void {
    core.info(`Parsing code file ${codeFilePath}`);
  }
}
