package eu.stamp_project.dspot.amplifier;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import eu.stamp_project.dspot.amplifier.amplifiers.Amplifier;
import eu.stamp_project.dspot.common.miscellaneous.DSpotUtils;
import spoon.reflect.declaration.CtMethod;

/**
 * Created by Benjamin DANGLOT
 * benjamin.danglot@inria.fr
 * on 19/07/18
 */
public class TextualDistanceInputAmplDistributor extends AbstractInputAmplDistributor {

    private static final Logger LOGGER = LoggerFactory.getLogger(TextualDistanceInputAmplDistributor.class);

    public TextualDistanceInputAmplDistributor(int maxNumTests, List<Amplifier> amplifiers) {
        super(maxNumTests, amplifiers);
    }

    /**
     * Input amplification for a single test.
     *
     * @param test Test method
     * @param i current iteration
     * @return New generated tests
     */
    protected Stream<CtMethod<?>> inputAmplifyTest(CtMethod<?> test, int i) {
        return this.amplifiers.parallelStream()
                .flatMap(amplifier -> amplifier.amplify(test, i));
    }

    /**
     * Input amplification for a single test.
     *
     * @param test Test method
     * @param i current iteration
     * @param targetMethodName the method need test
     * @return New generated tests
     */
    protected Stream<CtMethod<?>> inputAmplifyTest(CtMethod<?> test, int i, String targetMethodName) {
        return this.amplifiers.parallelStream()
                .flatMap(amplifier -> amplifier.amplify(test, i, targetMethodName));
    }

    /**
     * Input amplification of multiple tests.
     *
     * @param testMethods Test methods
     * @param i current iteration
     * @return New generated tests
     */
    @Override
    public List<CtMethod<?>> inputAmplify(List<CtMethod<?>> testMethods, int i) {
        LOGGER.info("Amplification of inputs...");
        List<CtMethod<?>> inputAmplifiedTests = testMethods.parallelStream()
                .flatMap(test -> {
                    final Stream<CtMethod<?>> inputAmplifiedTestMethods = inputAmplifyTest(test, i);
                    DSpotUtils.printProgress(testMethods.indexOf(test), testMethods.size());
                    return inputAmplifiedTestMethods;
                }).collect(Collectors.toList());
        LOGGER.info("{} new tests generated", inputAmplifiedTests.size());
        return reduce(inputAmplifiedTests);
    }

    @Override
    public List<CtMethod<?>> inputAmplify(List<CtMethod<?>> testMethods, int i, String targetMethodName){
        LOGGER.info("Amplification of inputs...");
        List<CtMethod<?>> inputAmplifiedTests = testMethods.parallelStream()
                .flatMap(test -> {
                    final Stream<CtMethod<?>> inputAmplifiedTestMethods = inputAmplifyTest(test, i, targetMethodName);
                    DSpotUtils.printProgress(testMethods.indexOf(test), testMethods.size());
                    return inputAmplifiedTestMethods;
                }).collect(Collectors.toList());
        LOGGER.info("{} new tests generated", inputAmplifiedTests.size());
        return reduce(inputAmplifiedTests);
    }

    /**
     * Reduces the number of amplified tests to a practical threshold.
     *
     * The reduction aims at keeping a maximum of diversity. Because all the amplified tests come from the same
     * original test, they have a <em>lot</em> in common.
     *
     * Diversity is measured with the textual representation of amplified tests. We use the sum of the bytes returned
     * by the {@link String#getBytes()} method and keep the amplified tests with the most distant values.
     *
     * @param tests List of tests to be reduced
     * @return A subset of the input tests
     */
    public List<CtMethod<?>> reduce(List<CtMethod<?>> tests) {
        final List<CtMethod<?>> reducedTests = new ArrayList<>();
        if (tests.size() > this.maxNumTests) {
            LOGGER.warn("Too many tests have been generated: {}", tests.size());
            final Map<Long, List<CtMethod<?>>> valuesToMethod = new HashMap<>();
            for (CtMethod<?> test : tests) {
                final long value = sumByteArrayToLong(test.toString().getBytes());
                if (!valuesToMethod.containsKey(value)) {
                    valuesToMethod.put(value, new ArrayList<>());
                }
                valuesToMethod.get(value).add(test);
            }
            final Long average = average(valuesToMethod.keySet());
            while (reducedTests.size() < this.maxNumTests) {
                final Long furthest = furthest(valuesToMethod.keySet(), average);
                reducedTests.add(valuesToMethod.get(furthest).get(0));
                if (valuesToMethod.get(furthest).isEmpty()) {
                    valuesToMethod.remove(furthest);
                } else {
                    valuesToMethod.get(furthest).remove(0);
                    if (valuesToMethod.get(furthest).isEmpty()) {
                        valuesToMethod.remove(furthest);
                    }
                }
            }
            LOGGER.info("Number of generated test reduced to {}", reducedTests.size());
        }
        if (reducedTests.isEmpty()) {
            reducedTests.addAll(tests);
        }
        return reducedTests;
    }

    /**
     * Returns the average of a collection of double
     */
    private Long average(Collection<Long> values) {
        return values.stream().collect(Collectors.averagingLong(Long::longValue)).longValue();
    }

    /**
     * Returns the first, most distant element of a collection from a defined value.
     */
    private Long furthest(Collection<Long> values, Long average) {
        return values.stream()
                .max(Comparator.comparingLong(d -> Math.abs(d - average)))
                .orElse(null);
    }

    private long sumByteArrayToLong(byte[] byteArray) {
        long sum = 0L;
        for (byte aByteArray : byteArray) {
            sum += (int) aByteArray;
        }
        return sum;
    }
}
