package kms.querybus.webservice;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

import kms.admin.administration.AdminException;
import kms.admin.administration.AdminManager;
import kms.querybus.querying.QueryManager;

/**
 * QueryServiceImpl
 * The implementation of the querying web service. This is the primary usage of the Knowledge Management System.
 * Once Knowledge Agents are developed in collaboration with the business user this becomes the business user's
 * logic platform. It will allow for the business user / their front-end applications to retrieve results
 * to the predefined logic procedures that are exposed in the Knowledge Agent.
 * The query method is designed to take an unordered collection of parameters to apply to the relevant 
 * procedure, however they must have the correct identifying names. The result is a collection of output values. 
 * The input parameters and result collection is in the form of an array of name-value String pairs
 * If an error occurs during the execution of a call to this web service query method, the response Map will
 * contain just one name-value string pair which is named 'validationMessage' and the value will give further
 * details
 * @author Stewart Sims
 *
 */
@WebService(serviceName = "QueryService",
        	portName="QueryPort",
        	targetNamespace = "http://localhost:8080/",
        	wsdlLocation = "WEB-INF/wsdl/QueryService.wsdl")
public class QueryServiceImpl implements QueryService {

	public static final String VALIDATION_KEY = "validationMessage";
	public static boolean firstCall = true;
	private Logger logger = Logger.getLogger("kms.querybus.webservice.QueryServiceImpl");
	
	@WebMethod
	public Object[][] queryKA(@WebParam (name="kaIdentifier") String kaIdentifier, 
							  @WebParam (name="procedure") String procedure,
							  @WebParam (name="parameters") String[][] parameterWrapper) {
		String validationMessage = "";
		//A mechanism for circumventing the delayed initial response of SWAPP-KMS 
		if(firstCall) {
			firstCall = false;
			queryKA(kaIdentifier, procedure, parameterWrapper);
			try {
				Thread.sleep(2000);
			} catch (InterruptedException e) {
				logger.log(Level.SEVERE, "", e);
			}
		}
		try {
			Map<String, Object[]> parameters = new HashMap<String, Object[]>();
			if(parameterWrapper != null) {
				ParameterPairConverter parameterPairConverterIn = new ParameterPairConverter(parameterWrapper);
				parameters = parameterPairConverterIn.getNameValuePairMap();
			} else {
				parameters = null;
			}
			//check parameters supplied
			if(kaIdentifier != null && !kaIdentifier.trim().equals("")
			   && procedure != null && !procedure.trim().equals("")
			   && parameters != null) {
				//check KA existence
				AdminManager adminManager = new AdminManager();
				if(adminManager.checkExistence(kaIdentifier)) {
					//check procedure existence and carry out query
					QueryManager queryManager = new QueryManager();
					if(queryManager.checkProcedureExistence(kaIdentifier, procedure, parameters.size())) {
						Map<String,Object[]> result = queryManager.queryKA(kaIdentifier, procedure, parameters);
						if(result == null) {
							validationMessage = "An error occured carrying out the KA procedure query";
						} else {
							ParameterPairConverter parameterPairConverterOut = new ParameterPairConverter(result);
							return parameterPairConverterOut.getNameValuePairArray();
						}
					} else {
						validationMessage = "Procedure " + procedure + " meta data could not be found - procedure does not exist with given parameters";
					}
				} else {
					validationMessage = "KA with identifier "+kaIdentifier+" does not exist";
				}
			} else {
				validationMessage = "Required input missing - check KA Identifier, procedure or parameters";
			}
			HashMap<String,Object[]> validationResponse = new HashMap<String,Object[]>();
			validationResponse.put(VALIDATION_KEY, new Object[] {VALIDATION_KEY, validationMessage});
			ParameterPairConverter parameterPairConverterOut = new ParameterPairConverter(validationResponse);
			return parameterPairConverterOut.getNameValuePairArray();
		} catch (Exception e) {
			logger.log(Level.SEVERE, "", e);
			validationMessage = "An unexpected error occured using web service";
			HashMap<String,Object[]> validationResponse = new HashMap<String,Object[]>();
			validationResponse.put(VALIDATION_KEY, new Object[] {VALIDATION_KEY, validationMessage});
			ParameterPairConverter parameterPairConverterOut = new ParameterPairConverter(validationResponse);
			return parameterPairConverterOut.getNameValuePairArray();
		}
	}
	
	@WebMethod
	public String[][] testReturn() {
		String[][] test = new String[2][2];
		test[0][0] = "Name1";
		test[0][1] = "Value1";
		test[1][0] = "Name2";
		test[1][1] = "Value2";
		return test;
	}
}
