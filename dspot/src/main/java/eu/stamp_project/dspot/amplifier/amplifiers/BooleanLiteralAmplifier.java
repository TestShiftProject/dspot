package eu.stamp_project.dspot.amplifier.amplifiers;

import org.apache.commons.compress.utils.Sets;
import spoon.reflect.code.CtExpression;
import spoon.reflect.code.CtLiteral;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.factory.Factory;
import java.util.Collections;
import java.util.Set;
import java.util.stream.Stream;

public class BooleanLiteralAmplifier extends AbstractLiteralAmplifier<Boolean> {

    @Override
    protected Set<CtExpression<Boolean>> amplify(CtExpression<Boolean> original, CtMethod<?> testMethod) {
        final Factory factory = testMethod.getFactory();
        if (((CtLiteral<Boolean>)original).getValue() == null){
            return Sets.newHashSet(
                    factory.createLiteral(true),
                    factory.createLiteral(false)
            );
        }
        return Collections.singleton(factory.createLiteral(!((CtLiteral<Boolean>)original).getValue()));
    }

    @Override
    public Stream<CtMethod<?>> amplify(CtMethod<?> testMethod, int iteration, String targetMethodName){
        return amplify(testMethod, iteration);
    }

    @Override
    protected String getSuffix() {
        return "litBool";
    }

    @Override
    protected Class<?> getTargetedClass() {
        return Boolean.class;
    }
}
