package eu.stamp_project.prettifier.testnaming;

import eu.stamp_project.prettifier.Prettifier;
import eu.stamp_project.prettifier.configuration.UserInput;
import eu.stamp_project.prettifier.testnaming.code2vec.Code2VecExecutor;
import eu.stamp_project.prettifier.testnaming.code2vec.Code2VecParser;
import eu.stamp_project.prettifier.testnaming.code2vec.Code2VecWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import spoon.reflect.declaration.CtMethod;

import java.util.ArrayList;
import java.util.List;

public class Code2VecTestRenamer implements Prettifier {

    private static final Logger LOGGER = LoggerFactory.getLogger(Code2VecTestRenamer.class);

    UserInput configuration;

    public Code2VecTestRenamer(UserInput configuration) {
        this.configuration = configuration;
    }

    @Override
    public List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified) {
        List<CtMethod<?>> prettifiedMethodList = new ArrayList<>();
        Code2VecWriter writer = new Code2VecWriter(configuration.getPathToRootOfCode2Vec());
        Code2VecParser parser = new Code2VecParser();
        Code2VecExecutor code2VecExecutor = null;
        try {
            code2VecExecutor = new Code2VecExecutor(
                    configuration.getPathToRootOfCode2Vec(),
                    configuration.getRelativePathToModelForCode2Vec(),
                    configuration.getTimeToWaitForCode2vecInMillis()
            );
            for (CtMethod<?> amplifiedTestMethodToBeRenamed : amplifiedTestsToBePrettified) {
                writer.writeCtMethodToInputFile(amplifiedTestMethodToBeRenamed);
                code2VecExecutor.run();
                final String code2vecOutput = code2VecExecutor.getOutput();
                final String predictedSimpleName = parser.parse(code2vecOutput);
                LOGGER.info("Code2Vec predicted {} for {} as new name", predictedSimpleName, amplifiedTestMethodToBeRenamed.getSimpleName());
                amplifiedTestMethodToBeRenamed.setSimpleName(predictedSimpleName);
                prettifiedMethodList.add(amplifiedTestMethodToBeRenamed);
            }
        } finally {
            if (code2VecExecutor != null) {
                code2VecExecutor.stop();
            }
        }
        return prettifiedMethodList;
    }
}
