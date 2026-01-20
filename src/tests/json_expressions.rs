/// Test runner for JSON expression test files in the expressions/ directory
///
/// Each JSON file has the format:
/// {
///   "description": "...",
///   "tests": [
///     {"expr": "(+ 1 2)", "expected": "3", "note": "optional note"},
///     {"expr": "(bad)", "expected": "error", "note": "error expected"}
///   ]
/// }

#[cfg(test)]
mod json_tests {
    use std::fs;
    use std::path::Path;
    use std::rc::Rc;
    use serde::Deserialize;

    use crate::lexemes;
    use crate::tokens;
    use crate::values::eval::eval;
    use crate::values::Value;

    #[derive(Deserialize, Debug)]
    struct TestFile {
        description: String,
        tests: Vec<TestCase>,
    }

    #[derive(Deserialize, Debug)]
    struct TestCase {
        expr: String,
        expected: String,
        note: Option<String>,
    }

    struct TestResult {
        passed: bool,
        expr: String,
        expected: String,
        actual: String,
        note: String,
    }

    fn eval_expr(code: &str) -> Result<Rc<Value>, String> {
        let mut chars = code.chars();
        let lexes = lexemes::get_lexemes(&mut chars).map_err(|e| e.to_string())?;
        let values = tokens::parse(lexes).map_err(|e| e.to_string())?;
        let env = Value::ground_env();

        values
            .into_iter()
            .map(|v| eval(v, env.clone()).map_err(|e| e.to_string()))
            .last()
            .unwrap_or_else(|| Ok(Value::make_inert()))
    }

    fn run_test_case(test: &TestCase) -> TestResult {
        let result = eval_expr(&test.expr);
        let note = test.note.clone().unwrap_or_default();

        if test.expected == "error" {
            match result {
                Err(e) => TestResult {
                    passed: true,
                    expr: test.expr.clone(),
                    expected: "error".to_string(),
                    actual: format!("error: {}", e),
                    note,
                },
                Ok(v) => TestResult {
                    passed: false,
                    expr: test.expr.clone(),
                    expected: "error".to_string(),
                    actual: v.to_string(),
                    note,
                },
            }
        } else {
            match result {
                Ok(value) => {
                    let actual = value.to_string();
                    TestResult {
                        passed: actual == test.expected,
                        expr: test.expr.clone(),
                        expected: test.expected.clone(),
                        actual,
                        note,
                    }
                }
                Err(e) => TestResult {
                    passed: false,
                    expr: test.expr.clone(),
                    expected: test.expected.clone(),
                    actual: format!("error: {}", e),
                    note,
                },
            }
        }
    }

    struct FileResults {
        file_name: String,
        description: String,
        passed: usize,
        failed: usize,
        failures: Vec<TestResult>,
    }

    fn run_test_file(path: &Path) -> FileResults {
        let content = fs::read_to_string(path).expect("Failed to read test file");
        let test_file: TestFile =
            serde_json::from_str(&content).expect("Failed to parse test file");

        let file_name = path.file_name().unwrap().to_str().unwrap().to_string();

        let mut passed = 0;
        let mut failed = 0;
        let mut failures: Vec<TestResult> = Vec::new();

        for test in &test_file.tests {
            let result = run_test_case(test);
            if result.passed {
                passed += 1;
            } else {
                failed += 1;
                failures.push(result);
            }
        }

        FileResults {
            file_name,
            description: test_file.description,
            passed,
            failed,
            failures,
        }
    }

    #[test]
    fn run_all_json_tests() {
        let expressions_dir = Path::new("expressions");

        if !expressions_dir.exists() {
            panic!("expressions/ directory not found");
        }

        let mut json_files: Vec<_> = fs::read_dir(expressions_dir)
            .expect("Failed to read expressions directory")
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().map_or(false, |ext| ext == "json"))
            .collect();

        json_files.sort();

        println!("\n========================================");
        println!("JSON Expression Test Runner");
        println!("========================================");
        println!("Found {} test files\n", json_files.len());

        let mut all_results: Vec<FileResults> = Vec::new();
        let mut total_passed = 0;
        let mut total_failed = 0;

        for path in &json_files {
            let results = run_test_file(path);
            total_passed += results.passed;
            total_failed += results.failed;

            let status = if results.failed == 0 { "✓" } else { "✗" };
            println!(
                "{} {} - {}/{} passed ({})",
                status,
                results.file_name,
                results.passed,
                results.passed + results.failed,
                results.description
            );

            all_results.push(results);
        }

        println!("\n========================================");
        println!("Summary: {}/{} tests passed", total_passed, total_passed + total_failed);
        println!("========================================");

        // Show failures grouped by file
        let files_with_failures: Vec<_> = all_results
            .iter()
            .filter(|r| !r.failures.is_empty())
            .collect();

        if !files_with_failures.is_empty() {
            println!("\nFailures:\n");

            for file_result in files_with_failures {
                println!("--- {} ({} failures) ---", file_result.file_name, file_result.failed);

                for (i, failure) in file_result.failures.iter().enumerate() {
                    if i >= 5 {
                        println!("  ... and {} more failures", file_result.failures.len() - 5);
                        break;
                    }
                    println!("  expr:     {}", failure.expr);
                    println!("  expected: {}", failure.expected);
                    println!("  actual:   {}", failure.actual);
                    if !failure.note.is_empty() {
                        println!("  note:     {}", failure.note);
                    }
                    println!();
                }
            }
        }

        assert_eq!(
            total_failed,
            0,
            "{} test(s) failed out of {}",
            total_failed,
            total_passed + total_failed
        );
    }
}
