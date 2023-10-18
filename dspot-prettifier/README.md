# DSpot Prettifier

This module enables you to perform a variety of post-processing steps on tests generated
by [DSpot](http://github.com/STAMP-project/dspot.git) to make them easier to understand, as well as filtering and
prioritizing the tests.

## Available Prettifiers

We provide different *prettifiers* that improve or modify the amplified test cases. They can be used separately or in
conjunction with each other. Here we will present all available prettifiers, how to enable them from the command line
and when they can be applied.

To learn how to configure the remaining options for the DSpot Prettifier, please consult the
section [Command Line Usage](##-command-line-usage). To learn about the prettifiers based on Code2Vec and Context2Name,
please consult the [readme-code2vec-context2name](readme-code2vec-context2name.md).

### Minimizers

The prettifier offers three minimizers:

#### General Minimizer `--apply-general-minimizer`

Removes redundant assertions and inlines local variables.

#### Pit Minimizer `--apply-pit-minimizer`

Removes assertions that do not improve the mutation score.

#### Extended Coverage Minimizer `--apply-extended-coverage-minimizer`

Removes statements that do not affect the coverage that a test case adds to the test suite. It is based on Oosterbroek
et al.'s minimizer from the paper "Removing Redundant Statements in Amplified Test Cases".  
(!) Requires the use of the ExtendedCoverageSelector during the amplification, and it's report to be available
under `--path-to-dspot-reports`.

### Dev-Friendly Filter `--filter-dev-friendly`

This prettifier filters the list of amplified test cases with the aim to remove test cases that developers do not find
useful.  
(!) Requires the use of the ExtendedCoverageSelector during the amplification, and it's report to be available
under `--path-to-dspot-reports`.  
(!) Requires the report of the modifications DSpot made to be available under `--path-to-dspot-reports`.

At the moment it sorts out:

- tests that only add coverage in simple getters or setters (simple = they only have one line of code). (because these
  could be seen as not necessary to test)
- tests that add coverage to methods named "hashCode" (because the current hashCode tests from DSpot compare against a
  plain value, which might be different on different machines)

It keeps (even if the other characteristics would filter these out):

- tests that check for an exception to be thrown (because exception paths are rarely tested, these are especially
  interesting for developers)

### Rename Test Methods `--rename-test-methods=...`

The prettifier provides two approaches to assign better names to the amplified test cases.

#### Improved Coverage Renamer `--rename-test-methods=ImprovedCoverageTestRenamer`

The ImprovedCoverageRenamer assigns new names to each test based on which unique methods it additionally covers. It is
based on the NATIC approach by Nijkamp et. al.'s approach from the paper "Naming Amplified Tests Based on Improved
Coverage".  
(!) Requires the use of the ExtendedCoverageSelector during the amplification, and it's report to be available
under `--path-to-dspot-reports`.

#### Code2Vec Test Renamer `--rename-test-methods=Code2VecTestRenamer`

This renamer uses [code2vec](https://github.com/tech-srl/code2vec) to learn better names for each test method. Have a
look at the [readme-code2vec-context2name](readme-code2vec-context2name.md) to learn how to set it up.

### Rename Local Variables `--rename-local-variables=...`

The prettifier provides two approaches to assign more readable names to the local variables in the amplified test cases.

#### Simple Variable Renamer `--rename-local-variables=SimpleVariableRenamer`

This renamer assigns each local variable with a name like "typeN" (string1, int3, ...).

#### Context2Name Variable Renamer `--rename-local-variables=Context2NameVariableRenamer`

This renamer uses [context2name](https://github.com/rbavishi/Context2Name) to learn better names for the local variables
inside the test methods. Have a look at the [readme-code2vec-context2name](readme-code2vec-context2name.md) to learn how
to set it up.

### Remove Redundant Casts `--remove-redundant-casts`

This prettifier aims to remove redundant casts from assertions.
(Not fully fledged out, in some cases casts remain.)

### Most Coverage Prioritizer `--prioritize-most-coverage`

This prioritization module re-orders the amplified test cases to put those on top that contribute the largest number of
additional covered instructions.  
(!) Requires the use of the ExtendedCoverageSelector during the amplification, and it's report to be available
under `--path-to-dspot-reports`.

### Description Generator `--generate-descriptions`

This module generates a short textual description of the test. Specifically: its modified behaviour, new assertion,
additionally covered methods and the original test case it was based on.  
(!) Requires the use of the ExtendedCoverageSelector during the amplification, and it's report to be available
under `--path-to-dspot-reports`.  
(!) Requires the report of the modifications DSpot made to be available under `--path-to-dspot-reports`.

## Command Line Usage

```
java -jar /path/to/dspot-prettifier-<VERSION>-jar-with-dependencies.jar --apply-all-prettifiers --absolute-path-to-project-root=<path> --path-to-amplified-test-class=<path>
```

The prettifier uses DSpot in the background, you can pass all arguments that you can also pass to DSpot. It always
processes one class of amplified test cases at once, and requires the absolute path to the corresponding java file (
option `--path-to-amplified-test-class`). It does not matter where this file is located relative to the project under
test. You also need to pass `--absolute-path-to-project-root` to specify the location of the project under test.

Some of the prettifiers require additional information that dspot reports during the original amplification.
With `--path-to-dspot-reports` you can point the prettifier to the folder containing these reports.
(By default both DSpot and the prettifier output to the `target` folder, which is overwritten at the start of both! So
you will have to copy DSpot's output to another folder to preserve it.)

```
--path-to-amplified-test-class=<pathToAmplifiedTestClass>
                             Specify the path to the java test class that has been amplified and that contains some
                             amplified test methods to be "prettified".If the test class is not in the package with
                             the standard test classes, you need to set this parameter!
--path-to-dspot-reports=<pathToDSpotReports>
                             Specify the path to the reporting jsons provided by DSpot. Default value:
                             target/dspot/output
```

### Configure Prettifiers

To select which prettifiers should be applied to the amplified test case you can pass these options:

```
--apply-all-prettifiers
                             Apply all available prettifiers. This overrides options that turn off specific prettifiers.
                             Default value: false
--apply-extended-coverage-minimizer
                             Apply the extended coverage minimizer to remove statements that do not affect the contributed coverage.
                             Default value: false
--apply-general-minimizer
                             Apply the general minimizer to remove redundant assertions and inline local variables.
                             Default value: false
--apply-pit-minimizer
                             Apply the pit minimizer to remove assertions that do not improve the mutation score.
                             Default value: false
--filter-dev-friendly
                             Filter the test cases according to which tests developers find useful.
                             Default value: false
--generate-descriptions
                             Generate textual descriptions of the test case's contributions.
                             Default value: false
--prioritize-most-coverage
                             Filter the test cases according to which tests developers find useful.
                             Default value: false
--remove-redundant-casts
                             Remove redundant casts in the generated assertions. Default value: false
--rename-local-variables=<variableRenamer>
                             Choose a VariableRenamer to give local variables new names.
                             Valid values: None, Context2NameVariableRenamer, SimpleVariableRenamer
                             Default value: None
--rename-test-methods=<testRenamer>
                             Choose a TestRenamer to give the tests new names.
                             Valid values: None, Code2VecTestRenamer, ImprovedCoverageTestRenamer
                             Default value: None
```
