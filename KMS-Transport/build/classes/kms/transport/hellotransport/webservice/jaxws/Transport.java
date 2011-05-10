
package kms.transport.hellotransport.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "transport", namespace = "http://localhost:8080/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "transport", namespace = "http://localhost:8080/")
public class Transport {

    @XmlElement(name = "queryPayload", namespace = "")
    private String queryPayload;

    /**
     * 
     * @return
     *     returns String
     */
    public String getQueryPayload() {
        return this.queryPayload;
    }

    /**
     * 
     * @param queryPayload
     *     the value for the queryPayload property
     */
    public void setQueryPayload(String queryPayload) {
        this.queryPayload = queryPayload;
    }

}
