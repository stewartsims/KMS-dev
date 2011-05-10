package kms.serviceclients.hello;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.xml.namespace.QName;
import javax.xml.ws.Service;

import kms.querybus.webservice.QueryServiceImpl;

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
 * @Author Stewart Sims
 */
public class InterfaceServiceClient {
	
	private static Logger logger = Logger.getLogger("kms.serviceclients.optimisation.InterfaceServiceClient");

	public static final String INTERFACE_PARAMETER_KA_IDENTIFIER = "hello";
	public static final String INTERFACE_PARAMETER_HELLO_PROCEDURE = "hello";
	public static final String INTERFACE_PARAMETER_HELLO_TRANSPORT_PROCEDURE = "helloTransport";
	
	public static final String INTERFACE_WSDL = "http://localhost:8080/KMS-Interface/QueryService?wsdl";
	public static final String INTERFACE_NAMESPACE = "http://localhost:8080/"; 
	public static final String INTERFACE_SERVICENAME = "QueryService";
	public static final String INTERFACE_PORTNAME = "QueryPort";
	
	public static QueryServiceImpl getInterfaceWebService() {
		String wsdlURL = INTERFACE_WSDL;
		String namespace = INTERFACE_NAMESPACE;
		String serviceName = INTERFACE_SERVICENAME;
		String portName = INTERFACE_PORTNAME;
		QName serviceQN = new QName(namespace, serviceName);
		QName portQN = new QName(namespace, portName);
		Service requestedService = null;
		try {
			requestedService = Service.create(new URL(wsdlURL), serviceQN);
		} catch (MalformedURLException e) {
			logger.log(Level.SEVERE, "Malformed URL for query web service");
		}
		QueryServiceImpl port = 
			(QueryServiceImpl) requestedService.getPort(portQN, QueryServiceImpl.class);
		return port;
	}
}
