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
    const indent = this.indentString.repeat(level ?? 0);
    const directoryPrefix = `${indent}- `;
    fs.readdir(directory, (derr, files) => {
      if (derr) {
        core.error(`Error reading directory ${directory}:`);
        return;
      }

      const fileIndent = this.indentString.repeat((level ?? 0) + 1);
      const filePrefix = `${fileIndent}- `;
      for (const file of files) {
        const filePath = path.join(directory, file);
        fs.stat(filePath, (ferr, stats) => {
          if (ferr) {
            core.error(`Error getting stats of file ${filePath}:`);
            return;
          }

          if (stats.isDirectory()) {
            core.debug(`${directoryPrefix}Directory: ${file}`);
            // Recursively read the directory
            this.readDirectory(filePath, (level ?? 0) + 1);
          } else if (stats.isFile()) {
            // Process the file
            core.debug(`${filePrefix}File: ${file}`);
          }
        });
      }
    });
  }
}
