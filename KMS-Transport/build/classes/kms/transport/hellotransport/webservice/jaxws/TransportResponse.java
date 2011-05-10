
package kms.transport.hellotransport.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "transportResponse", namespace = "http://localhost:8080/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "transportResponse", namespace = "http://localhost:8080/")
public class TransportResponse {

    @XmlElement(name = "dataObject", namespace = "")
    private Object dataObject;

    /**
     * 
     * @return
     *     returns Object
     */
    public Object getDataObject() {
        return this.dataObject;
    }

    /**
     * 
     * @param dataObject
     *     the value for the dataObject property
     */
    public void setDataObject(Object dataObject) {
        this.dataObject = dataObject;
    }

}
