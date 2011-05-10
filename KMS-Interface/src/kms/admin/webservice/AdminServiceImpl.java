package kms.admin.webservice;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;
import javax.xml.ws.WebServiceRef;

import kms.admin.administration.AdminException;
import kms.admin.administration.AdminManager;
import kms.knowledgeparser.webservice.ParserService;
import kms.knowledgeparser.webservice.ParserServiceImpl;

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
 * 
 * AdminServiceImpl
 * The implementation of the administration web service to administer the Knowledge Management System's
 * prolog back-end. It facilitates the creation, removal, modification and exportation of Knowledge Agents.
 * This web service is primarily intended for internal usage by the KMS owner.
 * @author Stewart Sims
 *
 */
@WebService(serviceName = "AdminService",
        	portName="AdminPort",
        	targetNamespace = "http://localhost:8080/",
        	wsdlLocation = "WEB-INF/wsdl/AdminService.wsdl")
public class AdminServiceImpl implements AdminService {
//	@WebServiceRef(wsdlLocation="http://localhost:8080/KMS-Interface/ParserService?wsdl")
//	private static ParserServiceImpl parserService;
	
	@WebMethod
	public String createKA(@WebParam(name="kaIdentifier") String kaIdentifier)
	{
		try {
			if(kaIdentifier != null && !kaIdentifier.trim().equals("")) {
				AdminManager adminManager = new AdminManager();
				boolean exists = adminManager.checkExistence(kaIdentifier);
				String result = "";
				if(exists) {
					return "KA with identifier " + kaIdentifier + " already exists";
				} else {
					boolean created = adminManager.newKA(kaIdentifier);
					if(created) {
						result += "KA "+kaIdentifier+ " created successfully \n";
					} else {
						throw new AdminException("KA could not be created");
					}
				}
				return result;
			} else {
				return "No kaIdentifier supplied";
			}
		} catch(AdminException e) {
			return "An exception occured: "+e;
		} 
	}
	@WebMethod
	public String removeKA(@WebParam(name="kaIdentifier") String kaIdentifier) {
		try {
			if(kaIdentifier != null && !kaIdentifier.trim().equals("")) {
				AdminManager adminManager = new AdminManager();
				boolean exists = adminManager.checkExistence(kaIdentifier);
				String result = "";
				if(!exists) {
					return "KA with identifier " + kaIdentifier + " does not exist";
				} else {
					boolean removed = adminManager.removeKA(kaIdentifier);
					if(removed) {
						result += "KA "+kaIdentifier+ " removed successfully \n";
					} else {
						throw new AdminException("KA could not be removed");
					}
				}
				return result;
			} else {
				return "No kaIdentifier supplied";
			}
		} catch(AdminException e) {
			return "An exception occured: "+e;
		} 
	}
	
	@WebMethod
	public String modifyKA(@WebParam(name="kaIdentifier") String kaIdentifier, 
						   @WebParam(name="logic") String logic) {
//		return parserServiceImpl.parseLogic(kaIdentifier, logic);
		return null;
	}
	@WebMethod
	public String exportKA(@WebParam(name="kaIdentifier") String kaIdentifier,
						   @WebParam(name="format") String format) {
		return "This method is not yet implemented on the web service";
	}
}
