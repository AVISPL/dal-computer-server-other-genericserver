/*
 * Copyright (c) 2025 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver.spring6;

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
 * ClientHttpResponse that works on Spring 6 and Spring 7
 * */
public class Spring6ClientHttpResponse implements ClientHttpResponse {
    private ByteArrayOutputStream buffer;
    private ClientHttpResponse response;
    private ObjectMapper mapper;
    private String contentType;

    public Spring6ClientHttpResponse(ByteArrayOutputStream buffer, ClientHttpResponse response, String contentType, ObjectMapper mapper) {
        this.buffer = buffer;
        this.response = response;
        this.contentType = contentType;
        this.mapper = mapper;
    }

    @Override
    public org.springframework.http.HttpStatusCode getStatusCode() throws IOException {
        return org.springframework.http.HttpStatusCode.valueOf(200);
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
        webServerResponse.setStatus(response.getStatusCode().value());
        webServerResponse.setContentType(contentType);

        return new ByteArrayInputStream(mapper.writeValueAsString(webServerResponse).getBytes());
    }

    @Override
    public HttpHeaders getHeaders() {
        // Copy instead of HttpHeaders.writableHttpHeaders(), which is removed in Spring 7
        HttpHeaders headers = new HttpHeaders();
        response.getHeaders().forEach(headers::put);
        headers.setContentType(MediaType.APPLICATION_JSON);
        return headers;
    }
}
