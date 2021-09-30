package com.foo.somedifferentpackage.examples.methodreplacement.subclass;

import org.evomaster.client.java.instrumentation.example.methodreplacement.subclass.SubclassExm;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by arcuri82 on 19-Sep-19.
 */
public class SubclassExmImp implements SubclassExm {


    @Override
    public String exe() {

        MyMap<String, Integer> map = new MyMap<>();

        //this call should not be replaced
        map.containsKey("foo");


        //this call should not be replaced
        MyIdentityMap<String, String> ident = new MyIdentityMap<>();
        ident.containsKey("bar");

        //replaced
        Map<String,String> regular = new HashMap<>();
        regular.containsKey("123");

        // this is as well replaced
        // TODO do we really want this behavior? to investigate
        Map<String, Integer> subclass = new MyMap<>();
        subclass.containsKey("456");

        return map.lastCheckedKey + ident.lastCheckedKey;
    }
}
