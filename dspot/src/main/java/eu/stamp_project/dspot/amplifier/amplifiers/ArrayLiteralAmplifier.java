package eu.stamp_project.dspot.amplifier.amplifiers;

import eu.stamp_project.dspot.common.test_framework.TestFramework;

import org.apache.commons.lang3.SerializationUtils;
import spoon.reflect.code.*;
import spoon.reflect.declaration.CtAnnotation;
import spoon.reflect.declaration.CtElement;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.factory.Factory;
import spoon.reflect.visitor.filter.TypeFilter;
import spoon.support.reflect.code.CtNewArrayImpl;

import java.lang.reflect.Array;
import java.util.*;
import java.util.stream.Stream;

/**
 * Created by Andrew Bwogi
 * abwogi@kth.se
 * on 12/09/19
 */
public class ArrayLiteralAmplifier extends AbstractLiteralAmplifier<CtNewArrayImpl> {

    @Override
    protected String getSuffix() {
        return "litArray";
    }

    @Override
    protected Class<?> getTargetedClass() {
        return Array.class;
    } // not used

    @Override
    protected List<CtExpression<CtNewArrayImpl>> getOriginals(CtMethod<?> testMethod) {
        return testMethod.getElements(ARRAY_LITERAL_TYPE_FILTER);
    }

    protected final TypeFilter<CtExpression<CtNewArrayImpl>> ARRAY_LITERAL_TYPE_FILTER = new TypeFilter<CtExpression<CtNewArrayImpl>>(CtExpression.class) {

        @Override
        public boolean matches(CtExpression<CtNewArrayImpl> candidate) {

            // keep only literals and array literals
            if (!(candidate instanceof CtLiteral || candidate instanceof CtNewArrayImpl)) {
                return false;
            }

            // don't keep elements of arrays
            if (candidate.getParent() instanceof CtNewArrayImpl) {
                return false;
            }
            if (candidate instanceof CtLiteral) {
                CtLiteral literal = (CtLiteral) candidate;
                try {

                    // don't keep candidates inside assertions and annotations
                    if ((literal.getParent() instanceof CtInvocation &&
                            TestFramework.get().isAssert((CtInvocation) literal.getParent()))
                            || isConcatenationOfLiteralInAssertion(literal)
                            || literal.getParent(CtAnnotation.class) != null) {
                        return false;
                    } else if (literal.getValue() == null) {
                        return getNullClass(literal).isArray();
                    }
                } catch (Exception e) {
                    // todo: maybe need a warning ?
                    return false;
                }
                return literal.getValue().getClass().isArray();
            } else {
                return true;
            }
        }

        private boolean isConcatenationOfLiteralInAssertion(CtLiteral literal) {
            CtElement currentElement = literal;
            while (currentElement.getParent() instanceof CtBinaryOperator) {
                currentElement = currentElement.getParent();
            }
            return currentElement.getParent() instanceof CtInvocation &&
                    TestFramework.get().isAssert((CtInvocation) literal.getParent());
        }
    };

    @Override
    protected Set<CtExpression<CtNewArrayImpl>> amplify(CtExpression<CtNewArrayImpl> original, CtMethod<?> testMethod) {
        final Factory factory = testMethod.getFactory();
        Set<CtExpression<CtNewArrayImpl>> values = new HashSet<>();

        // amplify an array set to null
        if (original instanceof CtLiteral && ((CtLiteral) original).getValue() == null) {
            String typeName = getNullClass((CtLiteral) original).getTypeName();
            String additionalElement = constructAdditionalElement(cropTypeName(typeName));
            String array = constructArray(typeName, additionalElement, false);
            CtExpression finalExpression = factory.createCodeSnippetExpression(array).compile();

            // create an array with one element and an empty array
            values.add(finalExpression);
            array = constructArray(typeName, "", true);
            finalExpression = factory.createCodeSnippetExpression(array).compile();
            values.add(finalExpression);
            return values;
        }
        CtNewArrayImpl castedOriginal = (CtNewArrayImpl) original;
        List<CtExpression> list = castedOriginal.getElements();

        // amplify an empty array
        if (list.isEmpty()) {
            String additionalElement = constructAdditionalElement(cropTypeName(original.getType().getSimpleName()));
            String array = constructArray(original.getType().toString(), additionalElement, false);
            CtExpression finalExpression = factory.createCodeSnippetExpression(array).compile();

            // create an array with one element and a null literal
            values.add(finalExpression);
            values.add(factory.createLiteral(null));
        } else {

            // amplify an array literal expression
            CtNewArray cloneAdd = SerializationUtils.clone(castedOriginal);
            CtNewArray cloneSub = SerializationUtils.clone(castedOriginal);
            List<CtExpression> elements = cloneSub.getElements();
            CtExpression newElement = SerializationUtils.clone(elements.get(0));
            cloneSub.removeElement(elements.get(0));
            cloneAdd.addElement(newElement);

            // create array expressions that are modifications of the original array expression and a null literal
            values.add(cloneAdd);
            values.add(cloneSub);
            if (list.size() > 1) {
                CtNewArray cloneEmpty = SerializationUtils.clone(castedOriginal);
                cloneEmpty.setElements(Collections.EMPTY_LIST);
                values.add(cloneEmpty);
            }
            values.add(factory.createLiteral(null));
        }
        return values;
    }

    public Stream<CtMethod<?>> amplify(CtMethod<?> testMethod, int iteration, String targetMethodName){
        return amplify(testMethod, iteration);
    }

    private String cropTypeName(String name) {
        int index = name.indexOf("[");
        name = name.substring(0, index);
        return name;
    }

    private String constructAdditionalElement(String type) {
        type = type.toLowerCase();
        if (type.equals("int") || type.equals("integer") || type.equals("short") || type.equals("byte")) {
            return "1";
        } else if (type.equals("long")) {
            return "1L";
        } else if (type.equals("float")) {
            return "1.1F";
        } else if (type.equals("double")) {
            return "1.1";
        } else if (type.equals("boolean")) {
            return "true";
        } else if (type.equals("char") || type.equals("character")) {
            return "'a'";
        } else if (type.equals("string")) {
            return "\"a\"";
        } else {
            return "null";
        }
    }

    private String constructArray(String type, String additionalElement, boolean isEmpty) {
        long dimensions;
        if (isEmpty) {
            dimensions = 1;
        } else {
            dimensions = type.chars().filter(num -> num == '[').count();
        }
        StringBuilder sb = new StringBuilder();
        sb.append("new " + type);
        for (int i = 0; i < dimensions; i++) {
            sb.append("{");
        }
        sb.append(additionalElement);
        for (int i = 0; i < dimensions; i++) {
            sb.append("}");
        }
        return sb.toString();
    }

    @Override
    public Stream<CtMethod<?>> amplify(CtMethod<?> testMethod, int iteration, String targetMethodName){
        return amplify(testMethod, iteration);
    }
}
