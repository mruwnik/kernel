/// Integration tests for JSON expression files using libtest-mimic
///
/// Each expression in each JSON file becomes its own test case.

use std::fs;
use std::rc::Rc;

use libtest_mimic::{Arguments, Failed, Trial};
use serde::Deserialize;

use orxl::lexemes;
use orxl::tokens;
use orxl::values::eval::eval;
use orxl::values::Value;

#[derive(Deserialize, Debug)]
struct TestFile {
    #[allow(dead_code)]
    description: String,
    tests: Vec<TestCase>,
}

#[derive(Deserialize, Debug, Clone)]
struct TestCase {
    expr: String,
    expected: String,
    note: Option<String>,
    #[serde(default)]
    skip: bool,
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

fn run_test_case(test: &TestCase) -> Result<(), Failed> {
    let result = eval_expr(&test.expr);

    let passed = if test.expected == "error" {
        result.is_err()
    } else {
        match &result {
            Ok(value) => value.to_string() == test.expected,
            Err(_) => false,
        }
    };

    if passed {
        Ok(())
    } else {
        let actual = match result {
            Ok(v) => v.to_string(),
            Err(e) => format!("error: {}", e),
        };
        Err(format!(
            "expr: {}\nexpected: {}\nactual: {}",
            test.expr, test.expected, actual
        )
        .into())
    }
}

fn make_test_name(file_stem: &str, test: &TestCase, index: usize) -> String {
    // Use note if available, otherwise use a sanitized version of the expression
    let suffix = match &test.note {
        Some(note) => sanitize_name(note),
        None => sanitize_name(&test.expr),
    };
    format!("{}::{:03}_{}", file_stem, index, suffix)
}

fn sanitize_name(s: &str) -> String {
    s.chars()
        .take(40)
        .map(|c| if c.is_alphanumeric() { c } else { '_' })
        .collect::<String>()
        .trim_matches('_')
        .to_lowercase()
}

fn collect_tests() -> Vec<Trial> {
    let expressions_dir = "expressions";
    let mut trials = Vec::new();

    let mut json_files: Vec<_> = fs::read_dir(expressions_dir)
        .expect("Failed to read expressions directory")
        .filter_map(|e| e.ok())
        .map(|e| e.path())
        .filter(|p| p.extension().map_or(false, |ext| ext == "json"))
        .collect();

    json_files.sort();

    for path in json_files {
        let file_stem = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        let content = fs::read_to_string(&path).expect("Failed to read test file");
        let test_file: TestFile =
            serde_json::from_str(&content).expect("Failed to parse test file");

        for (i, test) in test_file.tests.iter().enumerate() {
            if test.skip {
                continue;  // Skip tests marked with "skip": true
            }
            let test_name = make_test_name(&file_stem, test, i);
            let test_clone = test.clone();

            trials.push(Trial::test(test_name, move || run_test_case(&test_clone)));
        }
    }

    trials
}

fn main() {
    let args = Arguments::from_args();
    let tests = collect_tests();
    libtest_mimic::run(&args, tests).exit();
}
