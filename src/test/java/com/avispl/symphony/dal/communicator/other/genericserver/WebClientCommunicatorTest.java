/*
 * Copyright (c) 2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.Assert.assertNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.List;
import java.util.Map;

import com.avispl.symphony.api.dal.dto.monitor.Statistics;
import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.Rule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.dal.communicator.HttpCommunicator;
import com.avispl.symphony.dal.communicator.other.genericserver.utils.WebClientConstant;

import javax.security.auth.login.FailedLoginException;

/**
 * Unit test for {@link WebClientCommunicator}.
 * Success as 2xx in response status code for different content types such as html, xml, jpg; API Error for status code out of range 1xx to 5xx
 * URI with full path and short path, protocol as http and https
 *
 * @author Duy Nguyen, Ivan
 * @version 1.0.0
 * @since 1.2.0
 */
class WebClientCommunicatorTest {
	private static final int HTTP_PORT = 8088;
	private static final int HTTP_BASIC_AUTH_PORT = 65001;
	private static final int HTTPS_PORT = 8443;
	private static final String HOST_NAME = "127.0.0.1";
	private static final String PROTOCOL = "http";
	static WebClientCommunicator webClientCommunicator;

	@Rule
	WireMockRule wireMockRule = new WireMockRule(options().port(HTTP_PORT).httpsPort(HTTPS_PORT)
			.bindAddress(HOST_NAME));

	@BeforeEach
	void init() throws Exception {
		wireMockRule.start();
		webClientCommunicator = new WebClientCommunicator();
		webClientCommunicator.setTrustAllCertificates(true);
		webClientCommunicator.setTimeout(5000);
		webClientCommunicator.setProtocol(PROTOCOL);
		webClientCommunicator.setPort(wireMockRule.port());
		webClientCommunicator.setHost(HOST_NAME);
		webClientCommunicator.setContentType("text/plain");
		webClientCommunicator.setAuthenticationScheme(HttpCommunicator.AuthenticationScheme.None);
		webClientCommunicator.init();
	}

	@AfterEach
	void stopWireMockRule() {
		webClientCommunicator.destroy();
		wireMockRule.stop();
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()}  with API Error due to 100 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode100() {
		// Attempt to check accessible for index.html
		webClientCommunicator.setURI("/100");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to status code 100 is not supported");
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()}  with API Error due to 101 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode101() {
		// Attempt to check accessible for index.html
		webClientCommunicator.setURI("/101");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to status code 101 is not supported");
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()}  with API Error due to 102 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode102() {
		// Attempt to check accessible for index.html
		webClientCommunicator.setURI("/102");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to status code 102 is not supported");
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()}  with API Error due to 103 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode103() {
		// Attempt to check accessible for index.html
		webClientCommunicator.setURI("/103");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to status code 103 is not supported");
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for HTML content
	 */
	@Test
	void getMultipleStatisticsWithHtmlPage() throws Exception {
		// Attempt to check accessible for index.html
		webClientCommunicator.setURI("/");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		Assertions.assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for XML file
	 */
	@Test
	void getMultipleStatisticsWithXMLFile() throws Exception {
		// Attempt to check accessible for a xml file
		webClientCommunicator.setURI("/xml-file.xml");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for Jpg file
	 */
	@Test
	void getMultipleStatisticsWithJpgFile() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/jpg-file.jpg");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 201 Created
	 */
	@Test
	void getMultipleStatisticsWithStatusCode201() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/201");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("201 Created", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 202 Accepted
	 */
	@Test
	void getMultipleStatisticsWithStatusCode202() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/202");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("202 Accepted", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 203 Non-Authoritative Information
	 */
	@Test
	void getMultipleStatisticsWithStatusCode203() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/203");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("203 Non-Authoritative Information", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 204 No Content
	 */
	@Test
	void getMultipleStatisticsWithStatusCode204() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/204");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("204 No Content", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 205 Reset Content
	 */
	@Test
	void getMultipleStatisticsWithStatusCode205() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/205");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("205 Reset Content", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 206 Partial Content
	 */
	@Test
	void getMultipleStatisticsWithStatusCode206() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/206");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("206 Partial Content", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 207 Multi-Status (WebDAV)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode207() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/207");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("207 Multi-Status (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 208 Already Reported (WebDAV)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode208() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/208");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("208 Already Reported (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 226 IM Used (HTTP Delta encoding)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode226() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/226");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("226 IM Used (HTTP Delta encoding)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 403 Forbidden.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode403() throws Exception {
		webClientCommunicator.setURI("/forbidden");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("403 Forbidden", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 404 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode404() throws Exception {
		webClientCommunicator.setURI("/not-exist-uri");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("404 Not Found", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with default case that does not configure URI.
	 */
	@Test
	void getMultipleStatisticsWithDefault() throws Exception {
		// should be "Not Configured" when the URI are not set in Adapter Properties
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("Not Configured", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 401 Unauthorized
	 */
	@Test
	void getMultipleStatisticsWithStatusCode401() throws Exception {
		//  Should be "401 Unauthorized" when the URI are not set in Adapter Properties
		webClientCommunicator.setURI("invalid-login");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("401 Unauthorized", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 300 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode300() throws Exception {
		webClientCommunicator.setURI("/300");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("300 Multiple Choice", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 301 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode301() throws Exception {
		webClientCommunicator.setURI("/301");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("301 Moved Permanently", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 301 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode302() throws Exception {
		webClientCommunicator.setURI("/302");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("302 Found", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 303 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode303() throws Exception {
		webClientCommunicator.setURI("/303");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("303 See Other", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 304 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode304() throws Exception {
		webClientCommunicator.setURI("/304");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("304 Not Modified", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 305 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode305() throws Exception {
		webClientCommunicator.setURI("/305");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("305 Use Proxy", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 306 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode306() throws Exception {
		webClientCommunicator.setURI("/306");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("306 unused", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 307 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode307() throws Exception {
		webClientCommunicator.setURI("/307");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("307 Temporary Redirect", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 308 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode308() throws Exception {
		webClientCommunicator.setURI("/308");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("308 Permanent Redirect", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 500 Internal Server Error
	 */
	@Test
	void getMultipleStatisticsWithStatusCode500() throws Exception {
		webClientCommunicator.setURI("/internal-server-error");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("500 Internal Server Error", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 501 Not Implemented
	 */
	@Test
	void getMultipleStatisticsWithStatusCode501() throws Exception {
		webClientCommunicator.setURI("/not-implemented");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("501 Not Implemented", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 502 Bad Gateway
	 */
	@Test
	void getMultipleStatisticsWithStatusCode502() throws Exception {
		webClientCommunicator.setURI("/bad-gateway");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("502 Bad Gateway", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 503 Service Unavailable
	 */
	@Test
	void getMultipleStatisticsWithStatusCode503() throws Exception {
		webClientCommunicator.setURI("/service-unavailable");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("503 Service Unavailable", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 504 Gateway Timeout
	 */
	@Test
	void getMultipleStatisticsWithStatusCode504() throws Exception {
		webClientCommunicator.setURI("/gateway-timeout");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("504 Gateway Timeout", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 505 HTTP Version Not Supported
	 */
	@Test
	void getMultipleStatisticsWithStatusCode505() throws Exception {
		webClientCommunicator.setURI("/http-version-not-supported");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("505 HTTP Version Not Supported", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 506 Variant Also Negotiates
	 */
	@Test
	void getMultipleStatisticsWithStatusCode506() throws Exception {
		webClientCommunicator.setURI("/variant-also-negotiates");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("506 Variant Also Negotiates", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 507 Insufficient Storage (WebDAV)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode507() throws Exception {
		webClientCommunicator.setURI("/insufficient-storage");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("507 Insufficient Storage (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 508 Loop Detected (WebDAV)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode508() throws Exception {
		webClientCommunicator.setURI("/loop-detected");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("508 Loop Detected (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 510 Not Extended
	 */
	@Test
	void getMultipleStatisticsWithStatusCode510() throws Exception {
		webClientCommunicator.setURI("/not-extended");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("510 Not Extended", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 511 Network Authentication Required
	 */
	@Test
	void getMultipleStatisticsWithStatusCode511() throws Exception {
		webClientCommunicator.setURI("/network-authentication-required");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("511 Network Authentication Required", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and full path URI.
	 */
	@Test
	void getMultipleStatisticsWithFullPath() throws Exception {
		//  Expect 200 OK for full path of URI
		webClientCommunicator.setURI("http://localhost:8088/");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for status code 600 - out of range
	 */
	@Test
	void getMultipleStatisticsWithAPIError() throws IOException {
		webClientCommunicator.setURI("/out-range-http-status-code");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to status code 600 out of range 200");
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and https protocol being used.
	 */
	@Test
	void getMultipleStatisticsWithHttpsProtocol() throws Exception {
		//  Expect 200 OK for HTTPS protocol
		webClientCommunicator.destroy();
		webClientCommunicator.setProtocol("https");
		webClientCommunicator.setPort(wireMockRule.httpsPort());
		webClientCommunicator.setContentType("text/plain");
		webClientCommunicator.init();
		webClientCommunicator.setURI("/https");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for protocol not support on host.
	 */
	@Test
	void getMultipleStatisticsWithHttpsProtocolNotExisted() throws Exception {
		webClientCommunicator.destroy();
		webClientCommunicator.setProtocol("http");
		webClientCommunicator.setPort(wireMockRule.httpsPort());
		webClientCommunicator.setContentType("text/plain");
		webClientCommunicator.init();
		webClientCommunicator.setURI("https://127.0.0.1:443/");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to HTTPS protocol doesn't support on this host");
	}

	/**
	 * Test parse data by json object get from the request
	 *
	 * Expect JSON object parsing to be successful and status code 200 and Statistics of size 7
	 */
	@Test
	void testParseDataFromJsonObjectSuccessfully() throws Exception {
		webClientCommunicator.setURI("/device-json");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(7, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("Product 3", stats.get("Profile_information#ProdFullName"));
		assertEquals("714.4", stats.get("Profile_information#HardwareID"));
		assertEquals("3", stats.get("Profile_information#Brand"));
		assertEquals("3", stats.get("Device"));
		assertEquals("0, 2", stats.get("information"));
		assertEquals("the address 02", stats.get("address_name"));
	}

	/**
	 * Test parse data by json object get from the request and remove 2 field Brand and Version
	 *
	 * Expect size to be 5 stats because 2 Brand and Device fields in exclude were removed. Status code 200 and JSON object parsing successfully
	 */
	@Test
	void testParseDataFromJsonObjectAndRemoveTwoFieldInData() throws Exception {
		webClientCommunicator.setURI("/device-json");
		webClientCommunicator.setParseContent("true");
		webClientCommunicator.setExclude("Brand,Device");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(5, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertNull(stats.get("Profile_information#Brand"));
		assertNull("3", stats.get("Device"));
		assertEquals("Product 3", stats.get("Profile_information#ProdFullName"));
		assertEquals("714.4", stats.get("Profile_information#HardwareID"));
		assertEquals("0, 2", stats.get("information"));
		assertEquals("the address 02", stats.get("address_name"));
	}

	/**
	 * Test parse data by the key json is empty.
	 *
	 * Expect status code 200 and JSON object parsing successfully and haven't the key json empty in the statistics
	 */
	@Test
	void testParseDataWithTheKeyJsonIsEmpty() throws Exception {
		webClientCommunicator.setURI("/device-json-empty-key");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(3, stats.size());
		assertNull(stats.get(""));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("device 03", stats.get("Device_name"));
		assertEquals("the address 02", stats.get("address_name"));
	}

	/**
	 * Test parse data with key json has hash sign (#) in the key
	 *
	 * Expect parsing data successfully and remove the hash sign in key json
	 */
	@Test
	void testParseDataHasHashSignInTheKey() throws Exception {
		webClientCommunicator.setURI("/device-json-hash-sign");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(6, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("Product name", stats.get("Profile_Name#FullName"));
		assertEquals("331234", stats.get("Profile_Name#HardwareID"));
		assertEquals("Brand name", stats.get("Profile_Name#BrandName"));
		assertEquals("The address", stats.get("AddressName"));
		assertEquals("Device 03", stats.get("DeviceName"));
	}

	/**
	 * Test parse data with set exclude key json has hash sign (#) in the key
	 *
	 * Expect parsing data successfully and remove the hash sign in key json and remove 2 key is AddressName and HardwareID
	 */
	@Test
	void testParseDataWithExcludeHasHashSignInTheKey() throws Exception {
		webClientCommunicator.setURI("/device-json-hash-sign");
		webClientCommunicator.setParseContent("true");
		webClientCommunicator.setExclude("Address#Name,Hardware#ID");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(4, stats.size());
		assertNull(stats.get("AddressName"));
		assertNull(stats.get("Profile_Name#HardwareID"));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("Product name", stats.get("Profile_Name#FullName"));
		assertEquals("Brand name", stats.get("Profile_Name#BrandName"));
		assertEquals("Device 03", stats.get("DeviceName"));
	}

	/**
	 * Test parse data by get the request and response data is empty
	 *
	 * Expect call getMultipleStatistics throw exception because the data body is null
	 */
	@Test
	void testTheResponseDataIsEmpty() {
		webClientCommunicator.setURI("/device-empty");
		webClientCommunicator.setParseContent("true");

		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics().get(0), "Error when parsing data, the response is empty");
	}

	/**
	 * Test parse data by get the request and response data is array object
	 *
	 * Expect status code 200 and parsing unsupported data with field name Device_information as an array object
	 */
	@Test
	void testParseDataIsArrayObject() throws Exception {
		webClientCommunicator.setURI("/device-array-object");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(7, stats.size());
		assertNull(stats.get("Device_information"));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("Product 3", stats.get("Profile_information#ProdFullName"));
		assertEquals("714.4", stats.get("Profile_information#HardwareID"));
		assertEquals("3", stats.get("Profile_information#Brand"));
		assertEquals("3", stats.get("Device"));
		assertEquals("0, 2", stats.get("information"));
		assertEquals("the address 02", stats.get("address_name"));
	}

	/**
	 * Test parse data by get the request and response data is object multiple level
	 *
	 * Expect status code 200 parsing unsupported data with field name DataName as third level object
	 */
	@Test
	void testParseDataIsObjectMultipleLevel() throws Exception {
		webClientCommunicator.setURI("/device-object-and-object");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(7, stats.size());
		assertNull(stats.get("DataName"));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("Product 3", stats.get("Profile_information#ProdFullName"));
		assertEquals("714.4", stats.get("Profile_information#HardwareID"));
		assertEquals("3", stats.get("Profile_information#Brand"));
		assertEquals("3", stats.get("Device"));
		assertEquals("0, 2", stats.get("information"));
		assertEquals("the address 02", stats.get("address_name"));

	}

	/**
	 * Test parse data by get the request and response data as an array
	 *
	 * Expect status code 200 parsing data successfully with text array, number array and boolean array
	 */
	@Test
	void testParseDataXMLIsArray() throws Exception {
		webClientCommunicator.setURI("/device-array");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(4, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("0, 1, 2, 3, 4", stats.get("Number"));
		assertEquals("text 01, text 02, text 03", stats.get("Text"));
		assertEquals("true, false, true, false", stats.get("Boolean"));
	}

	/**
	 * Test parse data by xml data get from the request
	 *
	 * Expect xml data parsing to be successfully and status code 200 and Statistics of size 7
	 */
	@Test
	void testParseDataFromXmlDataSuccessfully() throws Exception {
		webClientCommunicator.setURI("/device-xml");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals(7, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("R00000EU", stats.get("Device_information#Article"));
		assertEquals("R9861522EU", stats.get("Device_information#Article_name"));
		assertEquals("C5010S", stats.get("Information#Model_name"));
		assertEquals("R9861522EU", stats.get("Information#Article_name"));
		assertEquals("true", stats.get("Information#Network"));
		assertEquals("C5010S", stats.get("Model"));
	}

	/**
	 * Test parse data by xml data get from the request and remove 3 field Network and Article
	 *
	 * Expect size to be 5 stats because 3 Article, Network and Model_name fields in exclude were removed. Status code 200 and xml data parsing successfully
	 */
	@Test
	void testParseDataFromXmlDataAndRemoveTwoFieldInData() throws Exception {
		webClientCommunicator.setURI("/device-xml");
		webClientCommunicator.setParseContent("true");
		webClientCommunicator.setExclude("Network,Model_name,Article");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals(4, stats.size());
		assertNull(stats.get("Device_information#Model_name"));
		assertNull(stats.get("Information#Network"));
		assertNull(stats.get("Device_information#Article"));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("R9861522EU", stats.get("Device_information#Article_name"));
		assertEquals("R9861522EU", stats.get("Information#Article_name"));
		assertEquals("C5010S", stats.get("Model"));
	}

	/**
	 * Test parsing data by getting request and response data as multiple identical child elements
	 *
	 * Expect status code 200 successfully parsing unsupported data with field name Device_name as array object
	 */
	@Test
	void testParseDataXMLWithMultipleIdenticalChildElements() throws Exception {
		webClientCommunicator.setURI("/device-xml-array-object");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(7, stats.size());
		assertNull(stats.get("Device_name"));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("R00000EU", stats.get("Device_information#Article"));
		assertEquals("R9861522EU", stats.get("Device_information#Article_name"));
		assertEquals("C5010S", stats.get("Information#Model_name"));
		assertEquals("R9861522EU", stats.get("Information#Article_name"));
		assertEquals("true", stats.get("Information#Network"));
		assertEquals("C5010S", stats.get("Model"));
	}

	/**
	 * Test parsing data by getting request and response data as multiple identical child elements as an array
	 *
	 * Expect status code 200 successfully parsing data with 2 field Device_name and Model_name as an array
	 */
	@Test
	void testParseDataXMLWithMSupportParseMultipleIdenticalChildElements() throws Exception {
		webClientCommunicator.setURI("/device-xml-array");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(8, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("100, 101, 102", stats.get("Information#Model_name"));
		assertEquals("C5010S, C5010S01, C5010S02", stats.get("Device_name"));
		assertEquals("R00000EU", stats.get("Device_information#Article"));
		assertEquals("R9861522EU", stats.get("Information#Article_name"));
		assertEquals("C5010S", stats.get("Model"));
		assertEquals("R9861522EU", stats.get("Device_information#Article_name"));
		assertEquals("true", stats.get("Information#Network"));
	}

	/**
	 * Test data parsing by getting request and response data with multiple subtag inside tag name
	 *
	 * Expect status code 200 parsing unsupported data with field name DataName as third level
	 */
	@Test
	void testParseDataXMLWithMultipleSubtagInsideTagName() throws Exception {
		webClientCommunicator.setURI("/device-tag-children");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals(7, stats.size());
		assertNull(stats.get("Device_name"));
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("R00000EU", stats.get("Device_information#Article"));
		assertEquals("R9861522EU", stats.get("Device_information#Article_name"));
		assertEquals("C5010S", stats.get("Information#Model_name"));
		assertEquals("R9861522EU", stats.get("Information#Article_name"));
		assertEquals("true", stats.get("Information#Network"));
		assertEquals("C5010S", stats.get("Model"));
	}

	/**
	 * Test content type multiple option and content valid parse content type
	 *
	 * Expect parsing data successfully
	 */
	@Test
	void testContentTypeMultipleOptionAndContentTypeValid() throws Exception {
		webClientCommunicator.setURI("/device-content-type-multiple-option");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals(6, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("Product 3", stats.get("Profile_information#ProdFullName"));
		assertEquals("714.4", stats.get("Profile_information#HardwareID"));
		assertEquals("3", stats.get("Profile_information#Brand"));
		assertEquals("3", stats.get("Device"));
		assertEquals("0, 2, 4, 6", stats.get("information"));
	}

	/**
	 * Test get content type from the request is empty
	 *
	 * Expect parsing data successfully
	 */
	@Test
	void testContentTypeIsEmpty() throws Exception {
		webClientCommunicator.setURI("/device-no-content");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals(6, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("brand name 3", stats.get("Profile#Brand_name"));
		assertEquals("device 03", stats.get("Device_name"));
		assertEquals("Product 3", stats.get("Profile#FullName"));
		assertEquals("714", stats.get("Profile#Hardware"));
		assertEquals("the address 02", stats.get("address_name"));
	}

	/**
	 * Test content type multiple option and content invalid
	 *
	 * Expect no parsing the data and  status code 200 OK
	 */
	@Test
	void testContentTypeMultipleOptionAndContentTypeInvalid() throws Exception {
		webClientCommunicator.setURI("/device-content-type-invalid");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(1, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test enter parseContent error type boolean
	 *
	 * Expect getMultipleStatistics throws exception with message enter parse content error
	 */
	@Test
	void testParseContentErrorThrowsException() {
		webClientCommunicator.setURI("/device-content-type-invalid");
		webClientCommunicator.setParseContent("true99999");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics().get(0),
				"The parseContent has a boolean data type (true or false). Please update parseContent: " + webClientCommunicator.getParseContent());
	}

	/**
	 * Test parseContent empty
	 *
	 * Expect getMultipleStatistics throws exception with message enter parse content error
	 */
	@Test
	void testParseContentIsEmptyErrorThrowsException() {
		webClientCommunicator.setURI("/device-content-type-invalid");
		webClientCommunicator.setParseContent("");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics().get(0), "The parseContent can't be empty. Please update the parseContent to true or false");
	}

	/**
	 * Test enter parseContent is false
	 *
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK and No parsing data
	 */
	@Test
	void testParseContentIsFalse() throws Exception {
		webClientCommunicator.setURI("/device-parse-content-false");
		webClientCommunicator.setParseContent("false");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals(1, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test the value more than 60 character
	 *
	 * Expect to truncate the value not more than 60 character
	 */
	@Test
	void testTheValueMoreThan60Character() throws Exception {
		webClientCommunicator.setURI("/device-xml-more-60-character");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals(7, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));
		assertEquals("R00000EU", stats.get("Device_information#Article"));
		assertEquals("C5010S", stats.get("Model"));
		assertEquals("C5010S", stats.get("Information#Model_name"));
		assertEquals("R9861522EU", stats.get("Device_information#Article_name"));
		assertEquals("R9861522EU", stats.get("Information#Article_name"));
		assertEquals("true", stats.get("Information#Network"));
	}

	/**
	 * Test duplicate key json level one
	 *
	 * Expect throw exception with message the key is duplicated
	 */
	@Test
	void testDuplicateKeyJsonLevelOne() {
		webClientCommunicator.setURI("/device-json-duplicate");
		webClientCommunicator.setParseContent("true");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), WebClientConstant.DUPLICATE_ERR);
	}

	/**
	 * Test duplicate key json level two
	 *
	 * Expect throw exception with message the key is duplicated
	 */
	@Test
	void testDuplicateKeyJsonLevelTwo() {
		webClientCommunicator.setURI("/device-json-duplicate-level-two");
		webClientCommunicator.setParseContent("true");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), WebClientConstant.DUPLICATE_ERR);
	}

	/**
	 * Test basic authorization error (missing username and password)
	 *
	 * @throws Exception if any unexpected error occurs
	 * @since 3.0.0
	 */
	@Test
	void testBasicAuthorizationError() throws Exception {
		webClientCommunicator.destroy();
		webClientCommunicator.setPort(80);
		webClientCommunicator.setURI("/json-object-third-level-basic-auth");
		webClientCommunicator.init();
		webClientCommunicator.setParseContent("true");
		assertThrows(FailedLoginException.class, () -> webClientCommunicator.getMultipleStatistics(), WebClientConstant.DUPLICATE_ERR);
	}

	/**
	 * Test basic authorization success
	 *
	 * @throws Exception if any unexpected error occurs
	 * @since 3.0.0
	 */
	@Test
	void testBasicAuthorizationSuccess() throws Exception {
		webClientCommunicator.destroy();
		webClientCommunicator.setPort(80);
		webClientCommunicator.setLogin("user");
		webClientCommunicator.setPassword("6b138015-bcc0-45c4-a403-73f9f83a2101");
		webClientCommunicator.setURI("/json-object-third-level-basic-auth");
		webClientCommunicator.init();

		webClientCommunicator.setParseContent("true");
		List<Statistics> statistics = webClientCommunicator.getMultipleStatistics();
		Assertions.assertNotNull(statistics);
		Assertions.assertNotNull(statistics.get(0));
		Map<String, String> stats = ((ExtendedStatistics) statistics.get(0)).getStatistics();
		Assertions.assertEquals("200 OK", stats.get("URI Status"));
		Assertions.assertEquals("CX-5010S", stats.get("Information#Model"));
		Assertions.assertEquals("RM09861522EU", stats.get("Information#Article"));
		Assertions.assertEquals("CXP-5000", stats.get("Information#Product"));
		Assertions.assertEquals("186355376", stats.get("Information#Serial"));
	}

	/**
	 * Test header authorization success (Custom-Auth-Header header)
	 *
	 * @throws Exception if any unexpected error occurs
	 * @since 3.0.0
	 */
	@Test
	void testHeaderAuthorizationSuccess() throws Exception {
		webClientCommunicator.destroy();
		webClientCommunicator.setPort(80);
		webClientCommunicator.setAuthorizationHeader("Custom-Auth-Header");
		webClientCommunicator.setPassword("MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3");
		webClientCommunicator.setURI("/json-object-third-level-token-auth");
		webClientCommunicator.init();

		webClientCommunicator.setParseContent("true");
		List<Statistics> statistics = webClientCommunicator.getMultipleStatistics();
		Assertions.assertNotNull(statistics);
		Assertions.assertNotNull(statistics.get(0));
		Map<String, String> stats = ((ExtendedStatistics) statistics.get(0)).getStatistics();
		Assertions.assertEquals("200 OK", stats.get("URI Status"));
		Assertions.assertEquals("CX-5010S", stats.get("Information#Model"));
		Assertions.assertEquals("RM09861522EU", stats.get("Information#Article"));
		Assertions.assertEquals("CXP-5000", stats.get("Information#Product"));
		Assertions.assertEquals("186355376", stats.get("Information#Serial"));
	}

	/**
	 * Test header authorization success with basic Authorization header and password including default bearer
	 *
	 * @throws Exception if any unexpected error occurs
	 * @since 3.0.0
	 */
	@Test
	void testHeaderAuthorizationSuccessWithBasicHeaderName() throws Exception {
		webClientCommunicator.destroy();
		webClientCommunicator.setPort(80);
		webClientCommunicator.setAuthorizationHeader("Authorization");
		webClientCommunicator.setPassword("Bearer MTQ0NjJkZmQ5OTM2NDE1ZTZjNGZmZjI3");
		webClientCommunicator.setURI("/json-object-third-level-token-auth-default-header-and-bearer");
		webClientCommunicator.init();

		webClientCommunicator.setParseContent("true");
		List<Statistics> statistics = webClientCommunicator.getMultipleStatistics();
		Assertions.assertNotNull(statistics);
		Assertions.assertNotNull(statistics.get(0));
		Map<String, String> stats = ((ExtendedStatistics) statistics.get(0)).getStatistics();
		Assertions.assertEquals("200 OK", stats.get("URI Status"));
		Assertions.assertEquals("CX-5010S", stats.get("Information#Model"));
		Assertions.assertEquals("RM09861522EU", stats.get("Information#Article"));
		Assertions.assertEquals("CXP-5000", stats.get("Information#Product"));
		Assertions.assertEquals("186355376", stats.get("Information#Serial"));
	}

	/**
	 * Test header authorization error (Wrong custom header name)
	 *
	 * @throws Exception if any unexpected error occurs
	 * @since 3.0.0
	 */
	@Test
	void testHeaderAuthorizationError() throws Exception {
		webClientCommunicator.destroy();
		webClientCommunicator.setPort(80);
		webClientCommunicator.setAuthorizationHeader("Custom-");
		webClientCommunicator.setPassword("MTQ0NjJkZmQ5OTM2NDE1ZT");
		webClientCommunicator.setURI("/json-object-third-level-token-auth");
		webClientCommunicator.init();

		webClientCommunicator.setParseContent("true");
		assertThrows(FailedLoginException.class, () -> webClientCommunicator.getMultipleStatistics(), WebClientConstant.DUPLICATE_ERR);
	}
}
