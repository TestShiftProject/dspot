package eu.stamp_project.dspot.amplifier.amplifiers;

import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtType;

import java.util.stream.Stream;

/**
 * Created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 07/10/19
 */
public class NoneAmplifier implements Amplifier {
    @Override
    public Stream<CtMethod<?>> amplify(CtMethod<?> testMethod, int iteration) {
        return Stream.empty();
    }
    public Stream<CtMethod<?>> amplify(CtMethod<?> testMethod, int iteration, String targetMethodName){
        return amplify(testMethod, iteration);
    }
    @Override
    public void reset(CtType<?> testClass) {

    }

    @Override
    public Stream<CtMethod<?>> amplify(CtMethod<?> testMethod, int iteration, String targetMethodName){
        return amplify(testMethod, iteration);
    }
}
