package eu.stamp_project.dspot.common.report;

import spoon.reflect.declaration.CtType;

/**
 * created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 08/04/19
 */
public interface Report {

    public void output(String outputDirectory);

    /**
     * Output the (partial) report for the given class.
     * Used to be able to use partial results if the DSpot process is killed before finishing.
     *
     * @param outputDirectory the path to the directory in which to write the reports.
     * @param testClass       the class of tests whose reports should be printed.
     */
    public void outputForClass(String outputDirectory, CtType<?> testClass);

    public void reset();

}
