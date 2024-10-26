import * as core from '@actions/core';
import { CodeParser } from './code-parsing/code-parser';

/**
 * The main function for the action.
 * @returns {Promise<void>} Resolves when the action is complete.
 */
export async function run(): Promise<void> {
  try {
    const sourceDirectory: string = core.getInput('source-directory');

    // Debug logs are only output if the `ACTIONS_STEP_DEBUG` secret is true
    core.debug(`Looking for source files in: ${sourceDirectory}`);

    // Parse the code in the source directory
    const codeParser = new CodeParser();
    codeParser.readDirectory(sourceDirectory);

    // Set outputs for other workflow steps to use
    core.setOutput('time', new Date().toTimeString());
  } catch (error) {
    // Fail the workflow run if an error occurs
    if (error instanceof Error) core.setFailed(error.message);
  }
}
