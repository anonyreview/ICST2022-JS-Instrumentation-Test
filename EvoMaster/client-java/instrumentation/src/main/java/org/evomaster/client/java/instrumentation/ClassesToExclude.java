package org.evomaster.client.java.instrumentation;

import org.evomaster.client.java.instrumentation.shared.ClassName;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.*;

public class ClassesToExclude {

    private static final Set<String> excludedClasses;
    private static final Set<String> includedClasses;

    static  {

        InputStream excludedClassesStream =
                ClassesToExclude.class.getClassLoader().getResourceAsStream("skipInstrumentationList.txt");

        excludedClasses = Collections.unmodifiableSet(new HashSet<>(getNotCommentedLines(excludedClassesStream)));

        InputStream includedClassesStream =
                ClassesToExclude.class.getClassLoader().getResourceAsStream("keepInstrumentationList.txt");

        includedClasses = Collections.unmodifiableSet(new HashSet<>(getNotCommentedLines(includedClassesStream)));
    }

    private static List<String> getNotCommentedLines(InputStream excludedClassesStream) {
        List<String> list = new ArrayList<>();

        try(BufferedReader br = new BufferedReader(new InputStreamReader(excludedClassesStream))){

            String line;
            while ((line = br.readLine()) != null) {
                String element = line.trim();
                if(! element.startsWith("//") && !element.isEmpty()) {
                    list.add(element);
                }
            }
        } catch(IOException e) {
            throw new RuntimeException(e);
        }

        return Collections.unmodifiableList(list);
    }

    public static Set<String> getPackagePrefixesShouldNotBeInstrumented() {
        return excludedClasses;
    }

    public static boolean checkIfCanInstrument(ClassName cn) {

        String className = cn.getFullNameWithDots();

        outer: for (String s : excludedClasses) {
            if (className.startsWith(s)) {

                for(String k : includedClasses){
                    if(className.startsWith(k)){
                        continue outer;
                    }
                }

                return false;
            }
        }

        return true;
    }
}
