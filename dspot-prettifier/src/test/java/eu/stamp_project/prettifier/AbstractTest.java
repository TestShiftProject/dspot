package eu.stamp_project.prettifier;

import eu.stamp_project.Utils;
import eu.stamp_project.dspot.amplifier.amplifiers.utils.RandomHelper;
import eu.stamp_project.dspot.amplifier.amplifiers.value.ValueCreator;
import eu.stamp_project.dspot.common.configuration.DSpotState;
import eu.stamp_project.dspot.common.configuration.options.AutomaticBuilderEnum;
import eu.stamp_project.prettifier.configuration.UserInput;
import eu.stamp_project.prettifier.output.report.ReportJSON;
import org.junit.jupiter.api.BeforeEach;

import java.util.Collections;

/**
 * Created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 1/31/17
 */
public abstract class AbstractTest {

    public String getAbsolutePathToProjectRoot() {
        return "src/test/resources/sample/";
    }

    public String getPathToDSpotOutput() {
        return "src/test/resources/sample/amplified-output";
    }

    protected UserInput configuration;

    @BeforeEach
    public void setUp() throws Exception {
        RandomHelper.setSeedRandom(72L);
        ValueCreator.count = 0;
        this.configuration = new UserInput();
        this.configuration.setAbsolutePathToProjectRoot(this.getAbsolutePathToProjectRoot());
        this.configuration.setTestClasses(Collections.singletonList("eu.stamp_project.AppTest"));
        this.configuration.setPathToDSpotReports(this.getPathToDSpotOutput());
        this.configuration.setVerbose(true);
        this.configuration.setBuilderEnum(AutomaticBuilderEnum.Maven);
        this.configuration.setGregorMode(true);
        DSpotState.verbose = true;
        Utils.init(this.configuration);
        Main.report = new ReportJSON(configuration);
    }
}
