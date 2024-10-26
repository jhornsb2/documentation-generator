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

export class CodeParser {
  readonly indentString = '  ';

  /**
   * Parses the provided code.
   * @param code The code to parse. This is expected to be the entire code of a file.
   */
  readDirectory(directory: string, level?: number): void {
    try {
      const indent = this.indentString.repeat(level ?? 0);
      const prefix = `${indent}- `;
      const directoryContents = fs.readdirSync(directory);
      const files: string[] = [];

      for (const file of directoryContents) {
        const filePath = path.join(directory, file);
        const stats = fs.statSync(filePath);

        if (stats.isDirectory()) {
          core.info(`${prefix}${file}`);
          // Recursively read the directory
          this.readDirectory(filePath, (level ?? 0) + 1);
        } else if (stats.isFile()) {
          files.push(file);
        }
      }
      for (const file of files) {
        // Process the file
        core.info(`${prefix}${file}`);
      }
    } catch (err) {
      console.error(`Error reading directory ${directory}:`, err);
    }
  }
}
