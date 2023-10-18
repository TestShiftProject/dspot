package eu.stamp_project.prettifier;

import spoon.reflect.declaration.CtMethod;

import java.util.List;

public interface Prettifier {

    /**
     * prettify the given list of test cases and return the prettified ones
     */
    List<CtMethod<?>> prettify(List<CtMethod<?>> amplifiedTestsToBePrettified);

}
