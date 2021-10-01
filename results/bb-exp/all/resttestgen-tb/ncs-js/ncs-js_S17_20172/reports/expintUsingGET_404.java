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

public class expintUsingGET_404 {

    private static ApiResponseParser apiResponseParser;

    @BeforeClass
    public static void beforeClass() throws IOException {
        apiResponseParser = new ApiResponseParser();
        
        
    }

	@Test
	public void expintUsingGET_TEST_404() throws Exception {
	{
		// Test Step expintUsingGET
	
		Integer integer151442075 = Integer.valueOf("0");
	
		Double double1238616099 = Double.valueOf("6.848813185392146E8");
	
		ExecutionResult executionresult70695990;
		try {
			// API Call
			Object returnValue_executionresult70695990 = new NcsRestApi().expintUsingGETWithHttpInfo(integer151442075,double1238616099);
			executionresult70695990 = apiResponseParser.parseApiResponseObject(ReflectionHelper.getMethodByName(NcsRestApi.class, "expintUsingGETWithHttpInfo"), returnValue_executionresult70695990);
		} catch (Exception e) {
			// Here, if request executed with http error code
			if (e.toString().contains("io.swagger.client.ApiException")){
				executionresult70695990 = apiResponseParser.parseApiExceptionObject(e);
			} else {
				throw new OperationExecutionException("Exception during the execution of operation", e);
			}
		}
		assertTrue(executionresult70695990.getStatusCode() == 404);
	
	}
	}

}
