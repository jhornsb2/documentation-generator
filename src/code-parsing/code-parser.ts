import * as core from '@actions/core';
import * as fs from 'fs';
import * as path from 'path';

const Parser = require('jison').Parser;

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
        core.info(`Parsing code file ${filePath}`);
        const fileContents = fs.readFileSync(filePath, 'utf-8');
        const grammar = {
          lex: {
            rules: [
              ['\\s+', 'console.log("Whitespace")'],
              ['[a-f0-9]+', "return 'HEX';"],
            ],
          },

          bnf: {
            hex_strings: ['hex_strings HEX', 'HEX'],
          },
        };

        // `grammar` can also be a string that uses jison's grammar format
        const parser = new Parser(grammar);

        // generate source, ready to be written to disk
        core.info(parser.generate());

        // you can also use the parser directly from memory

        // TODO: parser.parse(fileContents);

        // returns true
        core.info(
          parser.parse(
            `package com.documentation.generator.java.classes;

/**
 * A simple class to test the documentation generator.
 */
public class TestClass1 {

	/**
	 * A private field of type String.
	 */
	private String foo;

	/**
	 * A private field of type int.
	 */
	private int bar;

	/**
	 * Default constructor.
	 */
	public TestClass1() {
		this("", 0);
	}

	/**
	 * Constructor with all parameters.
	 * 
	 * @param foo The foo parameter.
	 * @param bar The bar parameter.
	 */
	public TestClass1(String foo, int bar) {
		this.foo = foo;
		this.bar = bar;
	}

	/**
	 * Getter for the foo field.
	 * 
	 * @return The foo field.
	 */
	public String getFoo() {
		return foo;
	}

	/**
	 * Setter for the foo field.
	 * 
	 * @param foo The new value for the foo field.
	 */
	public void setFoo(String foo) {
		this.foo = foo;
	}

	/**
	 * Getter for the bar field.
	 * 
	 * @return The bar field.
	 */
	public int getBar() {
		return bar;
	}

	/**
	 * Setter for the bar field.
	 * 
	 * @param bar The new value for the bar field.
	 */
	public void setBar(int bar) {
		this.bar = bar;
	}

}
`,
          ),
        );
      }
    }
  }
}
