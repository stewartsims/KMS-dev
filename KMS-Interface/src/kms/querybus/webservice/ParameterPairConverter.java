package kms.querybus.webservice;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

public class ParameterPairConverter {
	private Map<String, Object[]> map;
	private Object[][] parameterPairArray;
	
	private Logger logger = Logger.getLogger("kms.querybus.webservice.ParameterPairConverter");
	
	//required for serializability
	public ParameterPairConverter() { }
	
	public ParameterPairConverter(Map<String, Object[]> map) {
		this.map = map;
	}
	
	public ParameterPairConverter(Object[][] parameterPairArray) {
		this.parameterPairArray = parameterPairArray;
	}
	
	public Object[][] getNameValuePairArray() {
		if(this.parameterPairArray == null) {
			if(map != null && map.size() > 0) {
				Object[][] nameValuePairArray = new Object[map.size()][];
				int i = 0;
				for(String key : map.keySet()) {
					if(map.get(key) != null) {
						nameValuePairArray[i] = map.get(key);
						i++;
					}
				}
				this.parameterPairArray = nameValuePairArray;
				return nameValuePairArray;
			} else {
				return null;
			}
		} else {
			return this.parameterPairArray;
		}
	}
	
	public Map<String,Object[]> getNameValuePairMap() {
		if(this.map == null) {
			if(parameterPairArray != null && parameterPairArray.length > 0) {
				Map<String, Object[]> map = new HashMap<String, Object[]>();
				for(int i=0 ; i < parameterPairArray.length; i++) {
					if(parameterPairArray[i] != null && parameterPairArray[i].length > 0) {
						map.put( (String)parameterPairArray[i][0], parameterPairArray[i]);
					}
				}
				return map;
			} else {
				return null;
			}
		} else {
			return map;
		}
	}
}
