package org.evomaster.client.java.instrumentation.coverage.methodreplacement.classes;


import org.evomaster.client.java.instrumentation.coverage.methodreplacement.DistanceHelper;
import org.evomaster.client.java.instrumentation.coverage.methodreplacement.MethodReplacementClass;
import org.evomaster.client.java.instrumentation.coverage.methodreplacement.Replacement;
import org.evomaster.client.java.instrumentation.heuristic.Truthness;
import org.evomaster.client.java.instrumentation.shared.ReplacementType;
import org.evomaster.client.java.instrumentation.shared.StringSpecialization;
import org.evomaster.client.java.instrumentation.shared.StringSpecializationInfo;
import org.evomaster.client.java.instrumentation.staticstate.ExecutionTracer;

import java.util.Objects;

import static org.evomaster.client.java.instrumentation.coverage.methodreplacement.NumberParsingUtils.getParsingHeuristicValueForFloat;

public class DoubleClassReplacement implements MethodReplacementClass {

    @Override
    public Class<?> getTargetClass() {
        return Double.class;
    }


    @Replacement(type = ReplacementType.EXCEPTION, replacingStatic = true)
    public static double parseDouble(String input, String idTemplate) {

        if (ExecutionTracer.isTaintInput(input)) {
            ExecutionTracer.addStringSpecialization(input,
                    new StringSpecializationInfo(StringSpecialization.DOUBLE, null));
        }

        if (idTemplate == null) {
            return Double.parseDouble(input);
        }

        try {
            double res = Double.parseDouble(input);
            ExecutionTracer.executedReplacedMethod(idTemplate, ReplacementType.EXCEPTION,
                    new Truthness(1, DistanceHelper.H_NOT_NULL));
            return res;
        } catch (NumberFormatException | NullPointerException e) {
            double h = getParsingHeuristicValueForFloat(input);
            ExecutionTracer.executedReplacedMethod(idTemplate, ReplacementType.EXCEPTION,
                    new Truthness(h, 1));
            throw e;
        }
    }

    @Replacement(type = ReplacementType.BOOLEAN)
    public static boolean equals(Double caller, Object anObject, String idTemplate) {
        Objects.requireNonNull(caller);

        if (idTemplate == null) {
            return caller.equals(anObject);
        }

        final Truthness t;
        if (anObject == null || !(anObject instanceof Double)) {
            t = new Truthness(DistanceHelper.H_REACHED_BUT_NULL, 1d);
        } else {
            Double anotherDouble = (Double) anObject;
            if (caller.equals(anotherDouble)) {
                t = new Truthness(1d, DistanceHelper.H_NOT_NULL);
            } else {
                double base = DistanceHelper.H_NOT_NULL;
                double distance = DistanceHelper.getDistanceToEquality(caller, anotherDouble);
                double h = DistanceHelper.heuristicFromScaledDistanceWithBase(base, distance);
                t = new Truthness(h, 1d);
            }
        }

        ExecutionTracer.executedReplacedMethod(idTemplate, ReplacementType.BOOLEAN, t);
        return caller.equals(anObject);
    }

    @Replacement(type = ReplacementType.EXCEPTION, replacingStatic = true)
    public static double valueOf(String input, String idTemplate) {
        return parseDouble(input,idTemplate);
    }
}
