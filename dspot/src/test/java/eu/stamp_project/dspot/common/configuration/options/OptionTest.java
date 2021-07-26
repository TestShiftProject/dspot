package eu.stamp_project.dspot.common.configuration.options;

import eu.stamp_project.Main;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import picocli.CommandLine;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;

/**
 * Testing help options and error handling
 */
public class OptionTest {

    private final ByteArrayOutputStream outContent = new ByteArrayOutputStream();
    private final ByteArrayOutputStream errContent = new ByteArrayOutputStream();
    private final PrintStream originalOut = System.out;
    private final PrintStream originalErr = System.err;

    @Before
    public void setUpStreams() {
        System.setOut(new PrintStream(outContent));
        System.setErr(new PrintStream(errContent));
    }

    @After
    public void restoreStreams() {
        System.setOut(originalOut);
        System.setErr(originalErr);
    }

    @Test
    public void testHelpOption() {
        Main.main(new String[]{
                "--help"
        });
        String out = outContent.toString();
        Assert.assertTrue(out.contains("Usage: eu.stamp_project.Main")) ;
    }

    @Test
    public void testParseException() {
        Main.main(new String[]{
                "-p/x"
        });
        String err = errContent.toString();
        Assert.assertTrue(err.contains("Usage: eu.stamp_project.Main")) ;
        Assert.assertTrue(err.contains("Unknown option: '-p/x'"));
    }


    @Test
    public void testInvalidInputCaughtByPreChecking() {
        Main.main(new String[]{});
        String out = outContent.toString();
        Assert.assertTrue(out.contains("You did not provide the path to the root folder of your project, which is " +
                                       "mandatory.\n" +
                                       "eu.stamp_project.dspot.common.configuration.check.InputErrorException: Error " +
                                       "in the provided input. Please check your properties file and your " +
                                       "command-line options.")) ;
    }

}
