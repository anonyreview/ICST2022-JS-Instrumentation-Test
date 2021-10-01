import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import io.swagger.client.*;
import io.swagger.client.api.*;
import io.swagger.client.model.*;

import java.util.List;
import java.util.ArrayList;
import java.io.IOException;
import java.lang.reflect.InvocationTargetException;

import static org.junit.Assert.*;
import org.junit.*;
import java.math.*;

import com.bpr.resttestgen.helper.ApiResponseParser;
import com.bpr.resttestgen.helper.ReflectionHelper;
import com.bpr.resttestgen.models.ExecutionResult;
import com.bpr.resttestgen.testgenerator.exceptions.OperationExecutionException;

public class gammqUsingGET_404 {

    private static ApiResponseParser apiResponseParser;

    @BeforeClass
    public static void beforeClass() throws IOException {
        apiResponseParser = new ApiResponseParser();
        
        
    }

	@Test
	public void gammqUsingGET_TEST_404() throws Exception {
	{
		// Test Step gammqUsingGET
	
		Double double923341586 = Double.valueOf("58.89852174078566");
	
		Double double107994825 = Double.valueOf("7.915288089845653");
	
		ExecutionResult executionresult1470868839;
		try {
			// API Call
			Object returnValue_executionresult1470868839 = new NcsRestApi().gammqUsingGETWithHttpInfo(double923341586,double107994825);
			executionresult1470868839 = apiResponseParser.parseApiResponseObject(ReflectionHelper.getMethodByName(NcsRestApi.class, "gammqUsingGETWithHttpInfo"), returnValue_executionresult1470868839);
		} catch (Exception e) {
			// Here, if request executed with http error code
			if (e.toString().contains("io.swagger.client.ApiException")){
				executionresult1470868839 = apiResponseParser.parseApiExceptionObject(e);
			} else {
				throw new OperationExecutionException("Exception during the execution of operation", e);
			}
		}
		assertTrue(executionresult1470868839.getStatusCode() == 404);
	
	}
	}

}
