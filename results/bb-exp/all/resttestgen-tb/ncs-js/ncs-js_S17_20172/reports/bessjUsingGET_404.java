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

public class bessjUsingGET_404 {

    private static ApiResponseParser apiResponseParser;

    @BeforeClass
    public static void beforeClass() throws IOException {
        apiResponseParser = new ApiResponseParser();
        
        
    }

	@Test
	public void bessjUsingGET_TEST_404() throws Exception {
	{
		// Test Step bessjUsingGET
	
		Integer integer1877062907 = Integer.valueOf("-1952092273");
	
		Double double1670993182 = Double.valueOf("1.2222874355964675");
	
		ExecutionResult executionresult20804602;
		try {
			// API Call
			Object returnValue_executionresult20804602 = new NcsRestApi().bessjUsingGETWithHttpInfo(integer1877062907,double1670993182);
			executionresult20804602 = apiResponseParser.parseApiResponseObject(ReflectionHelper.getMethodByName(NcsRestApi.class, "bessjUsingGETWithHttpInfo"), returnValue_executionresult20804602);
		} catch (Exception e) {
			// Here, if request executed with http error code
			if (e.toString().contains("io.swagger.client.ApiException")){
				executionresult20804602 = apiResponseParser.parseApiExceptionObject(e);
			} else {
				throw new OperationExecutionException("Exception during the execution of operation", e);
			}
		}
		assertTrue(executionresult20804602.getStatusCode() == 404);
	
	}
	}

}
