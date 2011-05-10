package kms.transport.webservices;

import java.util.List;

import javax.ejb.Stateless;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceContext;
import javax.persistence.Query;

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

@Stateless
public class TransportManagerBean implements TransportManager {
	@PersistenceContext(unitName=Constants.PERSISTENCE_UNIT)
	private EntityManager entityManager;
	
//	private Logger logger = Logger.getLogger(CategoryManagerBean.class.getName());
	
	@Override
	public List<Transport> findAll() {
		Query findAllQuery = entityManager.createNamedQuery("transport.findAll");
		return findAllQuery.getResultList();
	}

	@Override
	public Transport findById(String id) {
		return entityManager.find(Transport.class, id);
	}

	@Override
	public Transport updateTransport(Transport transport) {
		return entityManager.merge(transport);
	}

	@Override
	public void removeTransport(Transport transport) {
		entityManager.remove(transport);
	}
	
}