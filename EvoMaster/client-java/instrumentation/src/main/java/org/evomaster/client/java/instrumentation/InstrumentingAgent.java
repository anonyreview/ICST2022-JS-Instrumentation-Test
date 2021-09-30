package org.evomaster.client.java.instrumentation;

import org.evomaster.client.java.instrumentation.external.AgentController;
import org.evomaster.client.java.instrumentation.shared.ClassName;
import org.evomaster.client.java.instrumentation.staticstate.ObjectiveRecorder;
import org.evomaster.client.java.utils.SimpleLogger;
import org.objectweb.asm.ClassReader;

import java.io.IOException;
import java.io.PrintWriter;
import java.lang.instrument.ClassFileTransformer;
import java.lang.instrument.IllegalClassFormatException;
import java.lang.instrument.Instrumentation;
import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.ProtectionDomain;
import java.util.Objects;

/**
 * Entry point for the JavaAgent that will do the bytecode instrumentation
 */
public class InstrumentingAgent {


    /**
     * WARN: static variable with dynamic state.
     * Forced to use it due to very special nature of how
     * JavaAgents are handled
     */
    private static Instrumentator instrumentator;

    private static String packagePrefixesToCover;

    private static boolean active = false;

    /**
     * This is called to init the JavaAgent when starting a new JVM, eg
     * the config of JavaAgent is passed by command line when the JVM starts.
     *
     * @param args
     * @param inst
     */
    public static void premain(String args, Instrumentation inst) {
        agentmain(args, inst);
    }

    /**
     * Actual method that is going to be called when the JavaAgent is started.
     * This is called to init the JavaAgent when attached to an already running JVM.
     *
     * @param agentArgs in this case, the {@code packagePrefixesToCover}
     * @param inst
     */
    public static void agentmain(String agentArgs, Instrumentation inst) {

        packagePrefixesToCover = agentArgs;
        instrumentator = new Instrumentator(packagePrefixesToCover);
        inst.addTransformer(new TransformerForTests());
        active = true;

        String port = System.getProperty(InputProperties.EXTERNAL_PORT_PROP);
        if (port != null) {
            SimpleLogger.info("Starting remote instrumenting Agent for packages: " + agentArgs);
            AgentController.start(Integer.parseInt(port));
        }

        String sqlDriver = System.getProperty(InputProperties.SQL_DRIVER);
        if (sqlDriver != null) {
            SimpleLogger.info("Initializing P6SPY with base driver " + sqlDriver);
            initP6Spy(sqlDriver);
        }

        String outputFile = System.getProperty(InputProperties.OUTPUT_FILE);
        if(outputFile != null){
            Runtime.getRuntime().addShutdownHook(new Thread(() -> {
                saveCoverageToDisk(outputFile);
            }));
        }
    }


    private static void saveCoverageToDisk(String outputFile) {

        try {
            SimpleLogger.info("Going to save coverage data to " + outputFile);

            ClassScanner.forceLoading(packagePrefixesToCover);

            Path path = Paths.get(outputFile);
            Files.deleteIfExists(path);
            Files.createFile(path);

            PrintWriter writer = new PrintWriter(outputFile, "UTF-8");
            ObjectiveRecorder.printCoveragePerTarget(writer);
            writer.close();

        } catch (IOException e) {
            SimpleLogger.error("Failed to save data to disk: "+e.getMessage());
            throw new RuntimeException(e);
        }

    }

    public static boolean isActive() {
        return active;
    }


    public static void changePackagesToInstrument(String packagePrefixesToCover) {
        instrumentator = new Instrumentator(packagePrefixesToCover);
    }

    public static void initP6Spy(String driver) {
        Objects.requireNonNull(driver);

        //see http://p6spy.readthedocs.io/en/latest/configandusage.html
        System.setProperty("p6spy.config.driverlist", driver);
        System.setProperty("p6spy.config.filter", "true");
        System.setProperty("p6spy.config.include", "select,insert,update,delete");
        System.setProperty("p6spy.config.autoflush", "true");
        System.setProperty("p6spy.config.appender", "com.p6spy.engine.spy.appender.StdoutLogger");
        System.setProperty("p6spy.config.jmx", "false");

        /*
            Note: this is a reference to a class in another module, although
            this module does NOT (and should not) reference it.
            Long story... see documentation on how P6Spy is used in EM.
         */
        System.setProperty("p6spy.config.logMessageFormat", "org.evomaster.client.java.databasespy.P6SpyFormatter");
    }


    private static class TransformerForTests implements ClassFileTransformer {

        private final static Method m;

        static {
            try {
                m = ClassLoader.class.getDeclaredMethod("findLoadedClass", String.class);
            } catch (NoSuchMethodException e) {
                throw new RuntimeException(e);
            }
            m.setAccessible(true);
        }

        private static boolean isAlreadyLoaded(ClassLoader loader, String classNameWithDots){
            try {
                return m.invoke(loader, classNameWithDots) != null;
            } catch (Exception e) {
                throw new RuntimeException(e);
            }
        }

        @Override
        public byte[] transform(ClassLoader loader, String className,
                                Class<?> classBeingRedefined,
                                ProtectionDomain protectionDomain,
                                byte[] classfileBuffer) throws IllegalClassFormatException {

            ClassName cn = ClassName.get(className);

            if (!ClassesToExclude.checkIfCanInstrument(cn) ||
                isAlreadyLoaded(loader, cn.getFullNameWithDots())) {
                return classfileBuffer;
            }

            ClassReader reader = new ClassReader(classfileBuffer);

            byte[] instrumented = instrumentator.transformBytes(loader, cn, reader);
            if(instrumented == null){
                return classfileBuffer;
            }
            return instrumented;
        }
    }
}
