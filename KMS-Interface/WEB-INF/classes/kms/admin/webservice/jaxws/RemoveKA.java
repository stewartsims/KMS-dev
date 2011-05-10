
package kms.admin.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "removeKA", namespace = "http://localhost:8080/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "removeKA", namespace = "http://localhost:8080/")
public class RemoveKA {

    @XmlElement(name = "kaIdentifier", namespace = "")
    private String kaIdentifier;

    /**
     * 
     * @return
     *     returns String
     */
    public String getKaIdentifier() {
        return this.kaIdentifier;
    }

    /**
     * 
     * @param kaIdentifier
     *     the value for the kaIdentifier property
     */
    public void setKaIdentifier(String kaIdentifier) {
        this.kaIdentifier = kaIdentifier;
    }

}
