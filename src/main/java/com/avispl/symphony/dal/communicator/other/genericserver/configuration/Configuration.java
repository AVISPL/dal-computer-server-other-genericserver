package com.avispl.symphony.dal.communicator.other.genericserver.configuration;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.List;

public class Configuration {
    @JsonProperty("configuration")
    private List<EndpointConfiguration> endpointConfiguration;

    /**
     * Retrieves {@link #endpointConfiguration}
     *
     * @return value of {@link #endpointConfiguration}
     */
    public List<EndpointConfiguration> getEndpointConfiguration() {
        return endpointConfiguration;
    }

    /**
     * Sets {@link #endpointConfiguration} value
     *
     * @param endpointConfiguration new value of {@link #endpointConfiguration}
     */
    public void setEndpointConfiguration(List<EndpointConfiguration> endpointConfiguration) {
        this.endpointConfiguration = endpointConfiguration;
    }
}
