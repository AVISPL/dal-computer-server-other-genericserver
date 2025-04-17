/*
 * Copyright (c) 2025 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver.spring5;

import com.avispl.symphony.dal.communicator.other.genericserver.data.WebServerResponse;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.http.client.ClientHttpResponse;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

/**
 * Spring 5 compatible version of ClientHttpResponse
 * */
public class Spring5ClientHttpResponse implements ClientHttpResponse {
    private ByteArrayOutputStream buffer;
    private ClientHttpResponse response;
    private ObjectMapper mapper;
    private String contentType;

    public Spring5ClientHttpResponse(ByteArrayOutputStream buffer, ClientHttpResponse response, String contentType, ObjectMapper mapper) {
        this.buffer = buffer;
        this.response = response;
        this.contentType = contentType;
        this.mapper = mapper;
    }

    @Override
    public org.springframework.http.HttpStatus getStatusCode() throws IOException {
        return org.springframework.http.HttpStatus.OK;
    }

    @Override
    public int getRawStatusCode() throws IOException {
        return 200;
    }

    @Override
    public String getStatusText() throws IOException {
        return "";
    }

    @Override
    public void close() {
    }

    @Override
    public InputStream getBody() throws IOException {
        WebServerResponse webServerResponse = new WebServerResponse();
        webServerResponse.setBody(buffer.toString(StandardCharsets.UTF_8.name()));
        webServerResponse.setStatus(response.getRawStatusCode());
        webServerResponse.setContentType(contentType);

        return new ByteArrayInputStream(mapper.writeValueAsString(webServerResponse).getBytes());
    }

    @Override
    public HttpHeaders getHeaders() {
        HttpHeaders headers = response.getHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        return headers;
    }
}
