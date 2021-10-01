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

public class fisherUsingGET_404 {

    private static ApiResponseParser apiResponseParser;

    @BeforeClass
    public static void beforeClass() throws IOException {
        apiResponseParser = new ApiResponseParser();
        
        
    }

	@Test
	public void fisherUsingGET_TEST_404() throws Exception {
	{
		// Test Step fisherUsingGET
	
		Integer integer1849015357 = Integer.valueOf("711891994");
	
		Integer integer484199463 = Integer.valueOf("3");
	
		Double double2008106788 = Double.valueOf("15.038720157280551");
	
		ExecutionResult executionresult1570470538;
		try {
			// API Call
			Object returnValue_executionresult1570470538 = new NcsRestApi().fisherUsingGETWithHttpInfo(integer1849015357,integer484199463,double2008106788);
			executionresult1570470538 = apiResponseParser.parseApiResponseObject(ReflectionHelper.getMethodByName(NcsRestApi.class, "fisherUsingGETWithHttpInfo"), returnValue_executionresult1570470538);
		} catch (Exception e) {
			// Here, if request executed with http error code
			if (e.toString().contains("io.swagger.client.ApiException")){
				executionresult1570470538 = apiResponseParser.parseApiExceptionObject(e);
			} else {
				throw new OperationExecutionException("Exception during the execution of operation", e);
			}
		}
		assertTrue(executionresult1570470538.getStatusCode() == 404);
	
	}
	}

}
