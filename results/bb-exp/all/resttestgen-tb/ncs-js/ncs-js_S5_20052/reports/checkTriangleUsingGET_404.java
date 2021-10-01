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

public class checkTriangleUsingGET_404 {

    private static ApiResponseParser apiResponseParser;

    @BeforeClass
    public static void beforeClass() throws IOException {
        apiResponseParser = new ApiResponseParser();
        
        
    }

	@Test
	public void checkTriangleUsingGET_TEST_404() throws Exception {
	{
		// Test Step checkTriangleUsingGET
	
		Integer integer490391704 = Integer.valueOf("47");
	
		Integer integer460570271 = Integer.valueOf("1");
	
	
		ExecutionResult executionresult1653361344;
		try {
			// API Call
			Object returnValue_executionresult1653361344 = new NcsRestApi().checkTriangleUsingGETWithHttpInfo(integer490391704,integer460570271,integer460570271);
			executionresult1653361344 = apiResponseParser.parseApiResponseObject(ReflectionHelper.getMethodByName(NcsRestApi.class, "checkTriangleUsingGETWithHttpInfo"), returnValue_executionresult1653361344);
		} catch (Exception e) {
			// Here, if request executed with http error code
			if (e.toString().contains("io.swagger.client.ApiException")){
				executionresult1653361344 = apiResponseParser.parseApiExceptionObject(e);
			} else {
				throw new OperationExecutionException("Exception during the execution of operation", e);
			}
		}
		assertTrue(executionresult1653361344.getStatusCode() == 404);
	
	}
	}

}
