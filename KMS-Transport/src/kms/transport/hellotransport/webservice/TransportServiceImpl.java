package kms.transport.hellotransport.webservice;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebResult;
import javax.jws.WebService;

/**
 * Copyright 2010 Stewart Sims
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 * @author Stewart Sims
 *
 */
@WebService(serviceName = "TransportService",
    	portName="TransportPort",
    	targetNamespace = "http://localhost:8080/",
    	wsdlLocation = "WEB-INF/wsdl/TransportService.wsdl")
public class TransportServiceImpl implements TransportService {

	@WebMethod
	@WebResult(name = "dataObject")
	public Object transport(@WebParam (name="queryPayload") String queryPayload) {
		return "Hello, " + queryPayload;
	}
	
}