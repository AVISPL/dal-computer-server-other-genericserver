/*
 * Copyright (c) 2025 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver.data;

/**
 * Custom webserver response format for inner data exchange in WebClientCommunicator
 *
 * @author Maksym.Rossiitsev/Symphony team
 * @since 3.0.1
 */
public class WebServerResponse {
    private int status;
    private String body;
    private String contentType;

    /**
     * Retrieves {@link #status}
     *
     * @return value of {@link #status}
     */
    public int getStatus() {
        return status;
    }

    /**
     * Sets {@link #status} value
     *
     * @param status new value of {@link #status}
     */
    public void setStatus(int status) {
        this.status = status;
    }

    /**
     * Retrieves {@link #body}
     *
     * @return value of {@link #body}
     */
    public String getBody() {
        return body;
    }

    /**
     * Sets {@link #body} value
     *
     * @param body new value of {@link #body}
     */
    public void setBody(String body) {
        this.body = body;
    }

    /**
     * Retrieves {@link #contentType}
     *
     * @return value of {@link #contentType}
     */
    public String getContentType() {
        return contentType;
    }

    /**
     * Sets {@link #contentType} value
     *
     * @param contentType new value of {@link #contentType}
     */
    public void setContentType(String contentType) {
        this.contentType = contentType;
    }
}
