package kms.querybus.querying;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import kms.knowledgeparser.scanning.ParsingException;
import kms.prolog.comms.PrologException;
import kms.prolog.comms.PrologFailException;
import kms.prolog.comms.PrologRESTClient;
import kms.querybus.webservice.QueryServiceImpl;

import org.json.simple.parser.ParseException;

/**
 * QueryManager
 * This class handles all queries from the query web service and carries them out against the prolog back-end. 
 * It uses HttpManager in kms.prolog.comms to send a JSON request via HTTP in order to query a particular procedure on a Knowledge Agent
 * It then parses the response returned and puts it into a Map of name-value string pairs
 * @TODO
 * - change to httpManager.queryPrologPOST, currently some EOF error with http manager
 * @author Stewart Sims
 * 
 */
public class QueryManager extends PrologRESTClient {
	private Logger logger = Logger.getLogger("kms.querybus.querying.QueryManager");
	
	private static final String GENERIC_VARIABLE_1 = "X";
	
	public QueryManager() {
		super();
	}
	
	public String queryLogic(String kaIdentifier, String logic) {
		try {
			String response = httpManager.queryPrologGET(kaIdentifier, logic);
			return response;
		} catch (Exception e) {
			logger.log(Level.SEVERE,"Querying native logic",e);
			return "An unexpected error occured while processing the update " +
					"this is indicative of a communication problem in the KMS";
		}
	}

	@SuppressWarnings("unchecked")
	public boolean checkProcedureExistence(String kaIdentifier, 
										   String procedure, 
										   int arity) {
		try {
			String response = httpManager.queryPrologGET(kaIdentifier, procedure+"_meta("+arity+","+GENERIC_VARIABLE_1+")");
//			logger.log(Level.SEVERE, "Right here's the JSON: " + response + " and your arity is: " + arity);
			return parseJSONBooleanResponse(response);
		} catch (ParsingException e) {
			logger.log(Level.SEVERE,"Parsing Exception checking procedure existence",e);
			return false;
		} catch (IOException e) {
			logger.log(Level.SEVERE,"IOException checking procedure existence",e);
			return false;
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception checking procedure existence",e);
			return false;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception checking procedure existence",e);
			return false;
		}
	}

	/**
	 * queryKA does the following:
	 * 1. Retrieve procedure meta data
	 * 2. Align the parameters and carry out the query
	 * 3. Parse output and put into map (including checking if error)
	 * 4. Return output
	 * @param kaIdentifier
	 * @param procedure
	 * @param parameters
	 * @return
	 */
	public Map<String, Object[]> queryKA(String kaIdentifier,
									   	 String procedure, 
									   	 Map<String, Object[]> parameters) {
		try {
			String parametersMetaResponse = 
				httpManager.queryPrologGET(kaIdentifier, procedure+"_meta(" + parameters.size() + ","+GENERIC_VARIABLE_1+")");
			List<String> parametersMeta = parseJSONSingleParameterList(parametersMetaResponse, GENERIC_VARIABLE_1);
			Object[] parameterValues = new String[parametersMeta.size()];
			int i=0;
			for(String parameterMeta : parametersMeta) {
				logger.log(Level.SEVERE, "Parameter: " + parameterMeta);
				logger.log(Level.SEVERE, "Value: " + parameters.get(parameterMeta));
				String[] parameterValue = (String[]) parameters.get(parameterMeta);
				if(parameterValue != null) {
					if(parameterValue.length > 2) {
						parameterValues[i] = parameterArrayToJSONArray(parameterValue);
					} else {
						parameterValues[i] = valueToJSONValue(parameterValue[1]);
					}
				}
				i++;
			}
			if(parameterValues.length == parametersMeta.size()) {
				//now all validation complete carry out query
				//first need to prepare the logic query string
				StringBuilder logicQueryString = new StringBuilder();
				logicQueryString.append(procedure);
				logicQueryString.append("(");
				for(int j=0; j < parameterValues.length; j++) {
					if(parameterValues[j] == null) {
						logicQueryString.append(parametersMeta.get(j));
					} else{
						logicQueryString.append(parameterValues[j]);
					}
					if(j == parameterValues.length-1) {
						logicQueryString.append(")");
					}
					else {
						logicQueryString.append(",");
					}
				}
				//carry out query and parse response
				logger.log(Level.SEVERE, "query string: " + logicQueryString.toString());
				String response = httpManager.queryPrologGET(kaIdentifier, logicQueryString.toString());
				logger.log(Level.SEVERE, "reponse was: " + response);
				Map<String, Object[]> result = null;
				try {
					result = parseJSONAllParametersIntoMap(response);
				} catch(PrologFailException e) {
					result = new HashMap<String, Object[]>();
					result.put(QueryServiceImpl.VALIDATION_KEY, new Object[] {QueryServiceImpl.VALIDATION_KEY, "Prolog returned false / fail"});
				}
				return result;
			} else {
				HashMap<String, Object[]> result = new HashMap<String, Object[]>();
				result.put(QueryServiceImpl.VALIDATION_KEY, new Object[] {QueryServiceImpl.VALIDATION_KEY, "Incorrect query parameters provided for KA procedure"});
				return result;
			}
		} catch (ParsingException e) {
			logger.log(Level.SEVERE,"Parsing Exception carrying out query",e);
			return null;
		} catch (IOException e) {
			logger.log(Level.SEVERE,"IOException carrying out query",e);
			return null;
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception carrying out query",e);
			return null;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception carrying out query",e);
			return null;
		}
	}
	
}
