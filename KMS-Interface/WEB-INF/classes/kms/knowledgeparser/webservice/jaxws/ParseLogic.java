
package kms.knowledgeparser.webservice.jaxws;

import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlElement;
import javax.xml.bind.annotation.XmlRootElement;
import javax.xml.bind.annotation.XmlType;

@XmlRootElement(name = "parseLogic", namespace = "http://localhost:8080/")
@XmlAccessorType(XmlAccessType.FIELD)
@XmlType(name = "parseLogic", namespace = "http://localhost:8080/", propOrder = {
    "kaIdentifier",
    "logic"
})
public class ParseLogic {

    @XmlElement(name = "kaIdentifier", namespace = "")
    private String kaIdentifier;
    @XmlElement(name = "logic", namespace = "")
    private String logic;

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

    /**
     * 
     * @return
     *     returns String
     */
    public String getLogic() {
        return this.logic;
    }

    /**
     * 
     * @param logic
     *     the value for the logic property
     */
    public void setLogic(String logic) {
        this.logic = logic;
    }

}
