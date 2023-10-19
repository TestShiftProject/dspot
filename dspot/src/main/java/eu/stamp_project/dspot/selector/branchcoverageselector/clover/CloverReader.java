package eu.stamp_project.dspot.selector.branchcoverageselector.clover;

import com.atlassian.clover.TestCaseInfoLookup;
import com.atlassian.clover.reporters.html.HtmlReporter;
import eu.stamp_project.dspot.selector.branchcoverageselector.Coverage;

import java.io.File;

/**
 * Created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 22/12/17
 */
public class CloverReader {

    private static final String ROOT_DIRECTORY = "/target/clover";

    private static final String DATABASE_FILE = "/clover.db";

    private static final String REPORT_DIRECTORY = "/report/";

    public volatile static Coverage coverage;

    /**
     * read the database initialize by {@link CloverExecutor}.
     *
     * @param directory path to the directory of Clover's result
     * @return a map, that associate test method names and the map of executed line in each classes
     */
    public Coverage read(String directory) {
        coverage = new Coverage();
        final File rootDirectoryOfCloverFiles = new File(directory + ROOT_DIRECTORY);
        System.out.println("Reading Clover data " + rootDirectoryOfCloverFiles.getAbsolutePath());
        HtmlReporter.runReport(new String[]{
                "-i", rootDirectoryOfCloverFiles.getAbsolutePath() + DATABASE_FILE,
                "-o", rootDirectoryOfCloverFiles.getAbsolutePath() + REPORT_DIRECTORY,
//                "--lineinfo",
//                "--showinner",
//                "--showlambda",
        });
        return coverage;
    }

}
