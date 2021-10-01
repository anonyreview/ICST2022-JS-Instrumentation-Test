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

public class remainderUsingGET_404 {

    private static ApiResponseParser apiResponseParser;

    @BeforeClass
    public static void beforeClass() throws IOException {
        apiResponseParser = new ApiResponseParser();
        
        
    }

	@Test
	public void remainderUsingGET_TEST_404() throws Exception {
	{
		// Test Step remainderUsingGET
	
		Integer integer260084831 = Integer.valueOf("4");
	
		Integer integer267814113 = Integer.valueOf("73");
	
		ExecutionResult executionresult598357562;
		try {
			// API Call
			Object returnValue_executionresult598357562 = new NcsRestApi().remainderUsingGETWithHttpInfo(integer260084831,integer267814113);
			executionresult598357562 = apiResponseParser.parseApiResponseObject(ReflectionHelper.getMethodByName(NcsRestApi.class, "remainderUsingGETWithHttpInfo"), returnValue_executionresult598357562);
		} catch (Exception e) {
			// Here, if request executed with http error code
			if (e.toString().contains("io.swagger.client.ApiException")){
				executionresult598357562 = apiResponseParser.parseApiExceptionObject(e);
			} else {
				throw new OperationExecutionException("Exception during the execution of operation", e);
			}
		}
		assertTrue(executionresult598357562.getStatusCode() == 404);
	
	}
	}

}
