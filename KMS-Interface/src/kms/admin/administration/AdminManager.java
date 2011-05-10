package kms.admin.administration;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.json.simple.parser.ParseException;

import kms.prolog.comms.PrologException;
import kms.prolog.comms.PrologRESTClient;

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
 * @author Stewart Sims
 *
 */
public class AdminManager extends PrologRESTClient{
	private Logger logger = Logger.getLogger("kms.prolog.admin.AdminManager");
	
	private static String KA_MANAGER_MODULE = "ka_manager:";
	private static String EXISTS_PREDICATE = "exists";
	private static String SAVE_PREDICATE = "saveKA";
	private static String NEW_PREDICATE = "newKA";
	private static String REMOVE_PREDICATE = "removeKA";
	
	public AdminManager() {
		super();
	}
	
	public String queryAdmin(String logic) throws AdminException {
		try {
			String response = httpManager.admin(logic);
			return response;
		} catch (Exception e) {
			logger.log(Level.SEVERE,"Exception querying via admin: ",e);
			throw new AdminException("An unexpected error occured while processing the update " +
					"this is indicative of a communication problem in the KMS");
		}
	}
	
	@SuppressWarnings("unchecked")
	public boolean checkExistence(String kaIdentifier) {
		try {
			String response = queryAdmin(EXISTS_PREDICATE+"("+kaIdentifier+")");
		    return parseJSONBooleanResponse(response);
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception checking KA existence",e);
			return false;
		} catch (AdminException e) {
			logger.log(Level.SEVERE,"Admin Exception checking KA existence",e);
			return false;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception checking procedure existence",e);
			return false;
		}
	}
	
	@SuppressWarnings("unchecked")
	public boolean saveKA(String kaIdentifier) {
		try {
			String response = queryAdmin(SAVE_PREDICATE+"("+kaIdentifier+")");
			return parseJSONBooleanResponse(response);
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception checking KA existence",e);
			return false;
		} catch (AdminException e) {
			logger.log(Level.SEVERE,"Admin Exception checking KA existence",e);
			return false;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception checking procedure existence",e);
			return false;
		}
	}
	
	@SuppressWarnings("unchecked")
	public boolean newKA(String kaIdentifier) {
		//Parse response for success message
		try {
			String response = queryAdmin(NEW_PREDICATE+"("+kaIdentifier+")");
			return parseJSONBooleanResponse(response);
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception checking KA existence",e);
			return false;
		} catch (AdminException e) {
			logger.log(Level.SEVERE,"Admin Exception checking KA existence",e);
			return false;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception checking procedure existence",e);
			return false;
		}
	}
	
	@SuppressWarnings("unchecked")
	public boolean removeKA(String kaIdentifier) {
		try {
			String response = queryAdmin(REMOVE_PREDICATE+"("+kaIdentifier+")");
			return parseJSONBooleanResponse(response);
		} catch (ParseException e) {
			logger.log(Level.SEVERE,"Parse Exception checking KA existence",e);
			return false;
		} catch (AdminException e) {
			logger.log(Level.SEVERE,"Admin Exception checking KA existence",e);
			return false;
		} catch (PrologException e) {
			logger.log(Level.SEVERE,"Prolog Exception checking procedure existence",e);
			return false;
		}
	}
}
