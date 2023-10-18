package eu.stamp_project;

import eu.stamp_project.dspot.common.miscellaneous.AmplificationHelper;
import eu.stamp_project.prettifier.Main;
import org.apache.commons.io.FileUtils;
import org.junit.jupiter.api.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.util.stream.Collectors;

public class MainTest {

    public static final String PATH_INPUT_TEST_CLASS = "src/test/resources/sample/src/test/java/fr/inria/amplified/AmplifiedTest.java";
    public static final String OUTPUT_PATH_AMPLIFIED_TEST = "target/dspot/output/fr/inria/amplified/AmplifiedTest.java";

    public static final String OUTPUT_PATH_TEST_SUITE_EXAMPLE = "target/dspot/output/example/TestSuiteExample2.java";
    public static final String REPORT_PATH_TEST_SUITE_EXAMPLE = "target/dspot/output/example" +
            ".TestSuiteExample2_prettifier_report.json";

    public static final String OUTPUT_PATH_APP_TEST = "target/dspot/output/eu/stamp_project/AppTest.java";
    public static final String REPORT_PATH_APP_TEST = "target/dspot/output/eu.stamp_project.AppTest_prettifier_report.json";

    @BeforeEach
    public void setUp() {
        try {
            FileUtils.deleteDirectory(new File("target/dspot/output/"));
            FileUtils.deleteDirectory(new File("src/test/resources/sample/target"));
        } catch (Exception ignored) {

        }
    }

    @AfterEach
    public void tearDown() {
        try {
            FileUtils.deleteDirectory(new File("target/dspot/output/"));
            FileUtils.deleteDirectory(new File("src/test/resources/sample/target"));
        } catch (Exception ignored) {

        }
    }

    @Test
    public void testNoPrettifiers() {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--path-to-amplified-test-class", PATH_INPUT_TEST_CLASS,
                "--test", "fr.inria.amplified.AmplifiedTest",
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_AMPLIFIED_TEST).exists());
    }

    @Test
    public void testApplyGeneralMinimizer() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--path-to-amplified-test-class", PATH_INPUT_TEST_CLASS,
                "--test", "fr.inria.amplified.AmplifiedTest",
                "--apply-general-minimizer"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_AMPLIFIED_TEST).exists());
        assertFileContains("Local variable was inlined", "assertEquals(5, 5);", OUTPUT_PATH_AMPLIFIED_TEST);
    }

    @Test
    public void testApplyPitMinimizer() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--path-to-amplified-test-class", PATH_INPUT_TEST_CLASS,
                "--test", "fr.inria.amplified.AmplifiedTest",
                "--apply-pit-minimizer"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_AMPLIFIED_TEST).exists());
        assertFileContains("Duplicate assertion was removed", "    @Test\n" +
                "    public void amplifiedTest2() throws Exception {\n" +
                "        Integer __DSPOT_1 = 5;\n" +
                "        assertEquals(5, __DSPOT_1.intValue());\n" +
                "    }", OUTPUT_PATH_AMPLIFIED_TEST);
    }

    @Test
    public void testImprovedCoverageTestNames() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--rename-test-methods=ImprovedCoverageTestRenamer",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "eu.stamp_project.AppTest",
                "--test-cases", "test1_mg12_assSep41,test1_mg13_failAssert0"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_APP_TEST).exists());
        assertFileContains("Name changed to covered methods in test", "testCompute", OUTPUT_PATH_APP_TEST);
        assertFileContains("Name changed to covered methods in test", "testThrowException", OUTPUT_PATH_APP_TEST);
        assertFileContains("New name included in json report", "testThrowException", REPORT_PATH_APP_TEST);
    }

    @Disabled // DOES NOT WORK ON TRAVIS, CANNOT FIND python3 cmd
    @Test
    public void testCode2VecTestNames() {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--path-to-amplified-test-class", PATH_INPUT_TEST_CLASS,
                "--test", "fr.inria.amplified.AmplifiedTest",
                "--path-to-code2vec", "src/test/resources/code2vec/code2vec",
                "--path-to-code2vec-model", "../model",
                "--rename-test-methods=Code2VecTestRenamer"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_AMPLIFIED_TEST).exists());
    }

    @Test
    public void testSimpleVariableNames() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--path-to-amplified-test-class", PATH_INPUT_TEST_CLASS,
                "--test", "fr.inria.amplified.AmplifiedTest",
                "--rename-local-variables=SimpleVariableRenamer"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_AMPLIFIED_TEST).exists());
        assertFileContains("Primitive was renamed to typeN", "int1", OUTPUT_PATH_AMPLIFIED_TEST);
        assertFileContains("Object type was renamed to typeN", "Integer1", OUTPUT_PATH_AMPLIFIED_TEST);
        assertFileContains("Non-DSpot named variables are not modified", "testingAnInt", OUTPUT_PATH_AMPLIFIED_TEST);
    }

    @Test
    public void testContext2VecLocalVariableNames() {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--path-to-amplified-test-class", PATH_INPUT_TEST_CLASS,
                "--test", "fr.inria.amplified.AmplifiedTest",
                "--path-to-code2vec", "src/test/resources/code2vec/code2vec",
                "--path-to-code2vec-model", "../model",
                "--rename-local-variables=Context2NameVariableRenamer"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_AMPLIFIED_TEST).exists());
    }

    @Test
    public void testTestDescriptions() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--generate-descriptions",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                //                "--test-cases", "test1_mg12_assSep41,test1_mg13_failAssert0"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
        assertFileContains("Added description to method", "* Test that", OUTPUT_PATH_TEST_SUITE_EXAMPLE);
        assertFileContains("Description included in json report", "Test that ", OUTPUT_PATH_TEST_SUITE_EXAMPLE);
    }

    @Test
    public void testTestDescriptionsAndCastRemover() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--generate-descriptions",
                "--remove-redundant-casts",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                //                "--test-cases", "test1_mg12_assSep41,test1_mg13_failAssert0"
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
        assertFileContains("Description has cast removed", "* Test that `ex.charAt", OUTPUT_PATH_TEST_SUITE_EXAMPLE);
    }


    @Test
    public void testFilterDevFriendly() {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                "--filter-dev-friendly",
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
    }

    @Test
    public void testPrioritizeMostCoverage() {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                "--prioritize-most-coverage",
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
    }

    @Test
    public void testExtendedCoverageMinimizer() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                "--apply-extended-coverage-minimizer",
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
        assertFileDoesNotContain("Redundant object creation removed", "char findChar = ex.charAt(\"abcd\", 3);",
                OUTPUT_PATH_TEST_SUITE_EXAMPLE);
    }

    @Test
    public void testRemoveRedundantCasts() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                "--remove-redundant-casts",
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
        assertFileDoesNotContain("Removed redundant cast to same type as expected in assertEquals", "(char)",
                OUTPUT_PATH_TEST_SUITE_EXAMPLE);
    }

    @Test
    public void testFullDevFriendlyPrettifier() throws Exception {
        Main.main(new String[]{
                "--absolute-path-to-project-root", "src/test/resources/sample/",
                "--with-comment", "All",
                "--path-to-dspot-reports", "src/test/resources/sample/amplified-output",
                "--test", "example.TestSuiteExample2",
                "--verbose",
                "--filter-dev-friendly",
                "--apply-general-minimizer",
                "--apply-extended-coverage-minimizer",
                "--rename-test-methods=ImprovedCoverageTestRenamer",
                "--rename-local-variables=SimpleVariableRenamer",
                "--remove-redundant-casts",
                "--generate-descriptions",
                "--prioritize-most-coverage",
        });
        Assertions.assertTrue(new File(OUTPUT_PATH_TEST_SUITE_EXAMPLE).exists());
        assertFileContains("Added description to method", "/**\n" +
                "     * Test that ", OUTPUT_PATH_TEST_SUITE_EXAMPLE);
        assertFileContains("Full prettified test case", " */\n" +
                "    @Test(timeout = 10000)\n" +
                "    public void testCharAt1() throws Exception {\n" +
                "        Example ex = new Example();\n" +
                "        Assert.assertEquals('?', ex.charAt(\"?i!rb0/|]6^FT)-ef&bk\", -839241703));\n" +
                "    }", OUTPUT_PATH_TEST_SUITE_EXAMPLE);
    }

    private void assertFileContains(String message, String expected, String path) throws Exception {
        final File amplifiedTestClass = new File(path);

        try (BufferedReader reader = new BufferedReader(new FileReader(amplifiedTestClass))) {
            String content = reader.lines().collect(Collectors.joining(AmplificationHelper.LINE_SEPARATOR));
            System.out.println(content);
            Assertions.assertTrue(content.contains(expected), message);
        }
    }

    private void assertFileDoesNotContain(String message, String forbidden, String path) throws Exception {
        final File amplifiedTestClass = new File(path);

        try (BufferedReader reader = new BufferedReader(new FileReader(amplifiedTestClass))) {
            String content = reader.lines().collect(Collectors.joining(AmplificationHelper.LINE_SEPARATOR));
            System.out.println(content);
            Assertions.assertFalse(content.contains(forbidden), message);
        }
    }
}
