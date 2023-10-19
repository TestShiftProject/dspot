package eu.stamp_project.dspot.amplifier;

import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtType;

import java.util.List;

/**
 * Created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 19/07/18
 */
public interface InputAmplDistributor {

    List<CtMethod<?>> inputAmplify(List<CtMethod<?>> testMethods, int iteration);

    List<CtMethod<?>> inputAmplify(List<CtMethod<?>> testMethods, int iteration, String TargetMethodName);

    void resetAmplifiers(CtType parentClass);

    boolean shouldBeRun();
}
