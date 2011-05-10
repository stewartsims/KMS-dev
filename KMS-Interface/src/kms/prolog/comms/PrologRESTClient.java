package kms.prolog.comms;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.json.simple.JSONArray;
import org.json.simple.JSONValue;
import org.json.simple.parser.ContainerFactory;
import org.json.simple.parser.JSONParser;
import org.json.simple.parser.ParseException;

public class PrologRESTClient {
	protected JSONParser jsonParser;
	protected ContainerFactory jsonContainerFactory;
	protected static HttpManager httpManager;
	
	private String PROLOG_FIELD_SUCCESS = "success";
	private String PROLOG_FIELD_MESSAGE = "message";
	private String PROLOG_FIELD_MESSAGE_POSITIVE = "yes";
	private String PROLOG_FIELD_ERROR = "error";
	private String PROLOG_FIELD_PARAMETERS = "bindings";
	
	public PrologRESTClient() {
		httpManager = new HttpManager();
		jsonParser = new JSONParser();
		jsonContainerFactory = new ContainerFactory(){
		    public List<Object> creatArrayContainer() {
		      return new LinkedList<Object>();
		    }

		    public Map<String,Object> createObjectContainer() {
		      return new LinkedHashMap<String,Object>();
		    }
		                        
		  };
	}
	
	@SuppressWarnings("unchecked")
	protected boolean parseJSONBooleanResponse(String response) 
		throws ParseException, PrologException {
	    Map<String,Object> json = (Map<String,Object>)jsonParser.parse(response, jsonContainerFactory);
	    Boolean success = (Boolean) json.get(PROLOG_FIELD_SUCCESS);
	    String message = (String) json.get(PROLOG_FIELD_MESSAGE);
	    if(success != null && message != null) {
		    if(success && message.equals(PROLOG_FIELD_MESSAGE_POSITIVE)) {
		    	return true;
		    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
		    	String error = (String) json.get(PROLOG_FIELD_ERROR);
		    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
		    } else {
		    	return false;
		    }
	    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
	    	String error = (String) json.get(PROLOG_FIELD_ERROR);
	    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
	    } else {
	    	return false;
	    }
	}
	
	//primarily for querying
	@SuppressWarnings("unchecked")
	protected String parseJSONSingleParameter(String response, String parameterName) 
		throws ParseException, PrologException {
	    Map<String,Object> json = (Map<String,Object>)jsonParser.parse(response, jsonContainerFactory);
	    Boolean success = (Boolean) json.get(PROLOG_FIELD_SUCCESS);
	    String message = (String) json.get(PROLOG_FIELD_MESSAGE);
	    if(success != null && message != null) {
		    if(success && message.equals(PROLOG_FIELD_MESSAGE_POSITIVE)) {
		    	List<Map> bindings = (List<Map>) json.get(PROLOG_FIELD_PARAMETERS);
		    	if(bindings != null) {
		    		List<String> parameterList = (List<String>) bindings.get(0).get(parameterName);
		    		return parameterList.get(0);
		    	} else {
		    		throw new PrologException("SWAPP-KMS response lacking parameter bindings");
		    	}
		    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
		    	String error = (String) json.get(PROLOG_FIELD_ERROR);
		    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
		    } else {
		    	throw new PrologFailException("SWAPP-KMS returned error: Query did not succeed");
		    }
	    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
	    	String error = (String) json.get(PROLOG_FIELD_ERROR);
	    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
	    } else {
	    	throw new PrologException("SWAPP-KMS returned error: Query did not return expected success and message response");
	    }
	}
	
	//primarily for querying
	@SuppressWarnings("unchecked")
	protected List<String> parseJSONSingleParameterList(String response, String parameterName) 
		throws ParseException, PrologException {
	    Map<String,Object> json = (Map<String,Object>)jsonParser.parse(response, jsonContainerFactory);
	    Boolean success = (Boolean) json.get(PROLOG_FIELD_SUCCESS);
	    String message = (String) json.get(PROLOG_FIELD_MESSAGE);
	    if(success != null && message != null) {
		    if(success && message.equals(PROLOG_FIELD_MESSAGE_POSITIVE)) {
		    	List<Map> bindings = (List<Map>) json.get(PROLOG_FIELD_PARAMETERS);
		    	if(bindings != null) {
		    		List<String> parameterList = (List<String>) bindings.get(0).get(parameterName);
		    		return parameterList;
		    	} else {
		    		throw new PrologException("SWAPP-KMS response lacking parameter bindings");
		    	}
		    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
		    	String error = (String) json.get(PROLOG_FIELD_ERROR);
		    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
		    } else {
		    	throw new PrologFailException("SWAPP-KMS returned error: Query did not succeed");
		    }
	    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
	    	String error = (String) json.get(PROLOG_FIELD_ERROR);
	    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
	    } else {
	    	throw new PrologException("SWAPP-KMS returned error: Query did not return expected success and message response");
	    }
	}
	
	//primarily for parsing SWAPP-KMS response to a query
	@SuppressWarnings("unchecked")
	protected Map<String, Object[]> parseJSONAllParametersIntoMap(String response) 
		throws ParseException, PrologException {
	    Map<String,Object> json = (Map<String,Object>)jsonParser.parse(response, jsonContainerFactory);
	    Boolean success = (Boolean) json.get(PROLOG_FIELD_SUCCESS);
	    String message = (String) json.get(PROLOG_FIELD_MESSAGE);
	    if(success != null && message != null) {
		    if(success && message.equals(PROLOG_FIELD_MESSAGE_POSITIVE)) {
		    	HashMap<String, Object[]> result = new HashMap<String, Object[]>();
		    	List<Map> bindings = (List<Map>) json.get(PROLOG_FIELD_PARAMETERS);
		    	if(bindings != null) {
		    		Map parameterBindingsMap = bindings.get(0);
		    		Set<String> keys = parameterBindingsMap.keySet();
		    		for(String key : keys) {
		    			Object valueObject = (Object) parameterBindingsMap.get(key);
		    			Object[] values = getReturnValues(key, valueObject);
		    			result.put(key, values);
    				}
		    		return result;
		    	} else {
		    		throw new PrologException("SWAPP-KMS response lacking parameter bindings");
		    	}
		    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
		    	String error = (String) json.get(PROLOG_FIELD_ERROR);
		    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
		    } else {
		    	throw new PrologFailException("SWAPP-KMS returned error: Query did not succeed");
		    }
	    } else if(json.get(PROLOG_FIELD_ERROR) != null && message != null) {
	    	String error = (String) json.get(PROLOG_FIELD_ERROR);
	    	throw new PrologException("SWAPP-KMS returned error: "+ error + " and message: " + message);
	    } else {
	    	throw new PrologException("SWAPP-KMS returned error: Query did not return expected success and message response");
	    }
	}

	private Object[] getReturnValues(String key, Object valueObject) {
		Object[] returnValues = new Object[2];
		if(key != null) {
			returnValues[0] = key;
		}
		if(valueObject instanceof List) {
			List<Object> values = (List<Object>) valueObject;
			if(key != null) {
				returnValues = new Object[values.size()+1];
				returnValues[0] = key;
			} else {
				returnValues = new Object[values.size()];
			}
			int i=1;
			for(Object value : values) {
				if(value instanceof List) {
//					List<Object> list = (List<Object>)value;
//					Object[][] array = new Object[list.size()][];
//					int j = 0;
//					for(Object object : list) {
//						if(object instanceof List) {
//							array[j] = ((List<Object>)object).toArray();
//							for(int k=0; k < array[j].length; k++) {
//								array[j][k] = objectToValue(object);
//							}
//						}
//						j++;
//					}
					returnValues[i] = objectToValue(value);
				} else {
					returnValues[i] = objectToValue(value);
				}
				i++;
			}
		} else {
			returnValues[1] = objectToValue(valueObject);
		}
		return returnValues;
	}
	
	private Object objectToValue(Object object) {
		if(object instanceof List) {
			return ((List<Object>)object).toArray();
		} else if(object instanceof String) {
			return (String) object;
		} else if(object instanceof Double) {
			return ((Double) object);
		} else if(object instanceof Integer) {
			return ((Integer) object);
		} else if(object instanceof Boolean) {
			return ((Boolean) object);
		} else if(object instanceof Number) {
			return ((Number) object);
		} else if(object instanceof Float) {
			return ((Float) object);
		} else {
			return null;
		}
	}
	
	public String parameterArrayToJSONArray(Object[] parameterArray) {
		JSONArray jsonArray = new JSONArray();
		for(int i=1; i < parameterArray.length; i++) {
			jsonArray.add(parameterArray[i]);
		}
		return jsonArray.toJSONString().replaceAll("\"", "");
	}
	
	public String valueToJSONValue(Object valueObject) {
		return JSONValue.toJSONString(valueObject).replaceAll("\"", "");
	}
}
