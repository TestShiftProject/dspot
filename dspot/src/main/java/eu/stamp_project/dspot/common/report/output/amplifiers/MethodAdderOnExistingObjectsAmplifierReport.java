package eu.stamp_project.dspot.common.report.output.amplifiers;

import eu.stamp_project.dspot.common.report.output.AmplifierReport;
import spoon.reflect.code.CtExpression;
import spoon.reflect.declaration.CtMethod;
import spoon.reflect.declaration.CtParameter;

import java.util.ArrayList;
import java.util.List;

/**
 * Reports the modification made by the
 * {@link eu.stamp_project.dspot.amplifier.amplifiers.MethodAdderOnExistingObjectsAmplifier}.
 */
public class MethodAdderOnExistingObjectsAmplifierReport extends AmplifierReport {

    private final MethodInvocation invokedMethod;

    public MethodAdderOnExistingObjectsAmplifierReport(CtMethod<?> invokedMethod, List<CtExpression<?>> arguments) {
        this.invokedMethod = new MethodInvocation(invokedMethod, arguments);
    }

    public MethodInvocation getInvokedMethod() {
        return invokedMethod;
    }

    @Override
    public boolean isAssertionReport() {
        return false;
    }

    /**
     * Represents the data relevant for one method invocation.
     */
    public static class MethodInvocation {

        // TODO: also save (simple) class name to give a better indication which method is meant
        private final String name;
        private final String returnType;
        List<MethodParameter> parameters;

        private MethodInvocation(CtMethod<?> invokedMethod, List<CtExpression<?>> arguments) {
            this.name = invokedMethod.getSimpleName();
            this.returnType = invokedMethod.getType().getSimpleName();
            this.parameters = new ArrayList<>(invokedMethod.getParameters().size());
            for (int i = 0; i < invokedMethod.getParameters().size(); i++) {
                this.parameters.add(new MethodParameter(invokedMethod.getParameters().get(i), arguments.get(i)));
            }
        }

        public String getName() {
            return name;
        }

        public String getReturnType() {
            return returnType;
        }

        public List<MethodParameter> getParameters() {
            return parameters;
        }
    }

    /**
     * Represents the data relevant for one parameter of a method invocation.
     */
    public static class MethodParameter {

        private final String name;
        private final String type;
        private final String value;

        /**
         * @param parameter the formal parameter
         * @param argument  the actual value passed to the parameter
         */
        private MethodParameter(CtParameter<?> parameter, CtExpression<?> argument) {
            this.name = parameter.getSimpleName();
            this.type = parameter.getType().toString();
            this.value = argument.toString();
        }

        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }

        public String getValue() {
            return value;
        }
    }
}
