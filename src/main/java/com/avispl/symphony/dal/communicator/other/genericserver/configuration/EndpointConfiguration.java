package com.avispl.symphony.dal.communicator.other.genericserver.configuration;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;
import java.util.Map;

public class EndpointConfiguration {
    private String endpoint;
    private Map<String, String> configuration;
    @JsonProperty("groups")
    private List<GroupConfiguration> groupConfiguration;

    /**
     * Retrieves {@link #endpoint}
     *
     * @return value of {@link #endpoint}
     */
    public String getEndpoint() {
        return endpoint;
    }

    /**
     * Sets {@link #endpoint} value
     *
     * @param endpoint new value of {@link #endpoint}
     */
    public void setEndpoint(String endpoint) {
        this.endpoint = endpoint;
    }

    /**
     * Retrieves {@link #configuration}
     *
     * @return value of {@link #configuration}
     */
    public Map<String, String> getConfiguration() {
        return configuration;
    }

    /**
     * Sets {@link #configuration} value
     *
     * @param configuration new value of {@link #configuration}
     */
    public void setConfiguration(Map<String, String> configuration) {
        this.configuration = configuration;
    }

    /**
     * Retrieves {@link #groupConfiguration}
     *
     * @return value of {@link #groupConfiguration}
     */
    public List<GroupConfiguration> getGroupConfiguration() {
        return groupConfiguration;
    }

    /**
     * Sets {@link #groupConfiguration} value
     *
     * @param groupConfiguration new value of {@link #groupConfiguration}
     */
    public void setGroupConfiguration(List<GroupConfiguration> groupConfiguration) {
        this.groupConfiguration = groupConfiguration;
    }
}
