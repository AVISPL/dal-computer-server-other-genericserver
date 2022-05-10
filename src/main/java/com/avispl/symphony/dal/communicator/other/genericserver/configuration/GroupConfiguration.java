package com.avispl.symphony.dal.communicator.other.genericserver.configuration;

import com.fasterxml.jackson.annotation.JsonProperty;

import java.util.Map;

public class GroupConfiguration {
    @JsonProperty("name")
    private String groupName;
    private Map<String, String> configuration;

    /**
     * Retrieves {@link #groupName}
     *
     * @return value of {@link #groupName}
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * Sets {@link #groupName} value
     *
     * @param groupName new value of {@link #groupName}
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
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
}
