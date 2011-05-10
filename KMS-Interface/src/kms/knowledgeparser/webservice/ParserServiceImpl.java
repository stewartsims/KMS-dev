package kms.knowledgeparser.webservice;

import javax.jws.WebMethod;
import javax.jws.WebParam;
import javax.jws.WebService;

import kms.admin.administration.AdminManager;
import kms.knowledgeparser.scanning.ScanManager;

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
 * ParserServiceImpl
 * The implementation of the knowledge parsing web service to administer updates to Knowledge Agents.
 * The updates are in the form of assertions to a particular Knowledge Agent module.
 * This web service may in the future become deprecated, depending on whether there is a need for
 * dynamic assertions to these modules.
 * @author Stewart Sims
 *
 */
@WebService(serviceName = "ParserService",
        	portName="ParserPort",
        	targetNamespace = "http://localhost:8080/",
        	wsdlLocation = "WEB-INF/wsdl/ParserService.wsdl")
public class ParserServiceImpl implements ParserService {
	@WebMethod
	public String parseLogic(@WebParam(name="kaIdentifier") String kaIdentifier, 
							 @WebParam(name="logic") String logic) {
		try {
			if(kaIdentifier != null && !kaIdentifier.trim().equals("")) {
				if(logic != null && !logic.trim().equals("")) {
					AdminManager adminManager = new AdminManager();
					boolean exists = adminManager.checkExistence(kaIdentifier);
					if(exists) {
						ScanManager scanManager = new ScanManager();
						boolean updated = scanManager.processNativeLogic(kaIdentifier+":"+"assert("+logic+").");
						boolean saved = adminManager.saveKA(kaIdentifier);
						if(saved && updated) {
							return "Update and save processed succesfully";
						} else {
							return "Failed, update: " + updated + " saved: " + saved;
						}
					} else {
						return "KA with identifier " + kaIdentifier + " could not be found";
					}
				} else {
					return "No logic to parse supplied";
				}
			} else {
				return "No kaIdentifier supplied";
			}
		} catch(Exception e) {
			return "An exception occured: "+e;
		}
	}
}
