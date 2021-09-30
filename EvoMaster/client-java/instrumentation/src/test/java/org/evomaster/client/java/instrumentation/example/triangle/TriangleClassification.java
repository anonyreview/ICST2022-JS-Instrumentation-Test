package org.evomaster.client.java.instrumentation.example.triangle;

public interface TriangleClassification {

    enum Classification {NOT_A_TRIANGLE, ISOSCELES, SCALENE, EQUILATERAL}


    Classification classify(int a, int b, int c);
}
