/*
 * Copyright (c) 2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
import static org.junit.Assert.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.IOException;
import java.util.Map;

import com.github.tomakehurst.wiremock.junit.WireMockRule;
import org.junit.Rule;
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.avispl.symphony.api.dal.dto.monitor.ExtendedStatistics;
import com.avispl.symphony.api.dal.error.ResourceNotReachableException;
import com.avispl.symphony.dal.communicator.HttpCommunicator;

/**
 * Unit test for {@link WebClientCommunicator}.
 * Success as 2xx in response status code for different content types such as html, xml, jpg; API Error for status code out of range 1xx to 5xx
 * URI with full path and short path, protocol as http and https
 *
 * @author Ivan
 * @version 1.0.0
 * @since 1.0.1
 */
class WebClientCommunicatorTest {
	private static final int HTTP_PORT = 8088;
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
	void getMultipleStatisticsWithHtmlPage() {
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
	void getMultipleStatisticsWithXMLFile() {
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
	void getMultipleStatisticsWithJpgFile() {
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
	void getMultipleStatisticsWithStatusCode201() {
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
	void getMultipleStatisticsWithStatusCode202() {
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
	void getMultipleStatisticsWithStatusCode203() {
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
	void getMultipleStatisticsWithStatusCode204() {
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
	void getMultipleStatisticsWithStatusCode205() {
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
	void getMultipleStatisticsWithStatusCode206() {
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
	void getMultipleStatisticsWithStatusCode207() {
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
	void getMultipleStatisticsWithStatusCode208() {
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
	void getMultipleStatisticsWithStatusCode226() {
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
	void getMultipleStatisticsWithStatusCode403() {
		webClientCommunicator.setURI("/forbidden");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("403 Forbidden", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 404 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode404() {
		webClientCommunicator.setURI("/not-exist-uri");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("404 Not Found", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with default case that does not configure URI.
	 */
	@Test
	void getMultipleStatisticsWithDefault() {
		// should be "Not Configured" when the URI are not set in Adapter Properties
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("Not Configured", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 401 Unauthorized
	 */
	@Test
	void getMultipleStatisticsWithStatusCode401() {
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
	void getMultipleStatisticsWithStatusCode300() {
		webClientCommunicator.setURI("/300");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("300 Multiple Choice", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 301 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode301() {
		webClientCommunicator.setURI("/301");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("301 Moved Permanently", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 301 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode302() {
		webClientCommunicator.setURI("/302");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("302 Found", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 303 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode303() {
		webClientCommunicator.setURI("/303");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("303 See Other", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 304 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode304() {
		webClientCommunicator.setURI("/304");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("304 Not Modified", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 305 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode305() {
		webClientCommunicator.setURI("/305");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("305 Use Proxy", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 306 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode306() {
		webClientCommunicator.setURI("/306");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("306 unused", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 307 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode307() {
		webClientCommunicator.setURI("/307");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("307 Temporary Redirect", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 308 response status code.
	 */
	@Test
	void getMultipleStatisticsWithStatusCode308() {
		webClientCommunicator.setURI("/308");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("308 Permanent Redirect", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 500 Internal Server Error
	 */
	@Test
	void getMultipleStatisticsWithStatusCode500() {
		webClientCommunicator.setURI("/internal-server-error");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("500 Internal Server Error", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 501 Not Implemented
	 */
	@Test
	void getMultipleStatisticsWithStatusCode501() {
		webClientCommunicator.setURI("/not-implemented");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("501 Not Implemented", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 502 Bad Gateway
	 */
	@Test
	void getMultipleStatisticsWithStatusCode502() {
		webClientCommunicator.setURI("/bad-gateway");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("502 Bad Gateway", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 503 Service Unavailable
	 */
	@Test
	void getMultipleStatisticsWithStatusCode503() {
		webClientCommunicator.setURI("/service-unavailable");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("503 Service Unavailable", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 504 Gateway Timeout
	 */
	@Test
	void getMultipleStatisticsWithStatusCode504() {
		webClientCommunicator.setURI("/gateway-timeout");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("504 Gateway Timeout", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 505 HTTP Version Not Supported
	 */
	@Test
	void getMultipleStatisticsWithStatusCode505() {
		webClientCommunicator.setURI("/http-version-not-supported");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("505 HTTP Version Not Supported", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 506 Variant Also Negotiates
	 */
	@Test
	void getMultipleStatisticsWithStatusCode506() {
		webClientCommunicator.setURI("/variant-also-negotiates");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("506 Variant Also Negotiates", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 507 Insufficient Storage (WebDAV)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode507() {
		webClientCommunicator.setURI("/insufficient-storage");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("507 Insufficient Storage (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 508 Loop Detected (WebDAV)
	 */
	@Test
	void getMultipleStatisticsWithStatusCode508() {
		webClientCommunicator.setURI("/loop-detected");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("508 Loop Detected (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 510 Not Extended
	 */
	@Test
	void getMultipleStatisticsWithStatusCode510() {
		webClientCommunicator.setURI("/not-extended");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("510 Not Extended", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 511 Network Authentication Required
	 */
	@Test
	void getMultipleStatisticsWithStatusCode511() {
		webClientCommunicator.setURI("/network-authentication-required");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("511 Network Authentication Required", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and full path URI.
	 */
	@Test
	void getMultipleStatisticsWithFullPath() {
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
	 * Expect status code 200 with content type is Json and parse json object successfully
	 */
	@Test
	void testParseDataFromJsonObjectSuccessfully() {
		webClientCommunicator.setURI("/device-json");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(7, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));

		for (Map.Entry<String, String> entry : stats.entrySet()) {
			assertNotNull(entry.getValue());
		}
	}

	/**
	 * Test parse data by json object get from the request and remove 2 field Brand and Version
	 *
	 * Expect size to be 5 stats because 2 Brand and Version fields in Exclusions were removed. Status code 200 with the content type of JSON and JSON object parsing successfully
	 */
	@Test
	void testParseDataFromJsonObjectAndRemoveTwoFieldInData() {
		webClientCommunicator.setURI("/device-json");
		webClientCommunicator.setParseContent("true");
		webClientCommunicator.setExclude("Brand, ,Device");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(5, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));

		for (Map.Entry<String, String> entry : stats.entrySet()) {
			assertNotNull(entry.getValue());
		}
	}

	/**
	 * Test parse data by get the request and response data is empty
	 *
	 * Expect status code 200 with content type invalid
	 */
	@Test
	void testTheResponseDataIsEmpty() {
		webClientCommunicator.setURI("/device-empty");
		webClientCommunicator.setParseContent("true");

		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics().get(0), "Error parsing data");
	}

	/**
	 * Test parse data by get the request and response data is array object
	 *
	 * Expect status code 200 with content type invalid
	 */
	@Test
	void testParseDataIsArrayObject() {
		webClientCommunicator.setURI("/device-array-object");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test parse data by get the request and response data is object multiple level
	 *
	 * Expect status code 200 with content type invalid
	 */
	@Test
	void testParseDataIsObjectMultipleLevel() {
		webClientCommunicator.setURI("/device-object-and-object");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));

	}

	/**
	 * Test parse data by get the request and response data is array object
	 *
	 * Expect status code 200 with content type invalid
	 */
	@Test
	void testParseDataXMLIsArray() {
		webClientCommunicator.setURI("/device-array");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test parse data by Xml data from the request
	 *
	 * Expect status code 200 with content type is Xml and parse Xml data successfully
	 */
	@Test
	void testParseDataFromXmlDataSuccessfully() {
		webClientCommunicator.setURI("/device-xml");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(5, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));

		for (Map.Entry<String, String> entry : stats.entrySet()) {
			assertNotNull(entry.getValue());
		}
	}

	/**
	 * Test parsing data by getting request and response data as multiple identical child elements
	 *
	 * Expect status code 200 with content type invalid
	 */
	@Test
	void testParseDataXMLWithMultipleIdenticalChildElements() {
		webClientCommunicator.setURI("/device-xml-array");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));

	}

	/**
	 * Test data parsing by getting request and response data with multiple subtag inside tag name
	 *
	 * Expect does not support parse data and status code 200 with content type invalid
	 */
	@Test
	void testParseDataXMLWithMultipleSubtagInsideTagName() {
		webClientCommunicator.setURI("/device-tag-children");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test content type multiple option and content valid parse content type
	 *
	 * Expect parsing data successfully with content type valid
	 */
	@Test
	void testContentTypeMultipleOptionAndContentTypeValid() {
		webClientCommunicator.setURI("/device-content-type-multiple-option");
		webClientCommunicator.setParseContent("true");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals(6, stats.size());
		assertEquals("200 OK", stats.get("URI Status"));

		for (Map.Entry<String, String> entry : stats.entrySet()) {
			assertNotNull(entry.getValue());
		}
	}

	/**
	 * Test content type multiple option and content invalid
	 *
	 * Expect parsing data with content type valid
	 */
	@Test
	void testContentTypeMultipleOptionAndContentTypeInvalid() {
		webClientCommunicator.setURI("/device-content-type-invalid");
		webClientCommunicator.setParseContent("true");

		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics().get(0), "Error the content type invalid");
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
				"The parseContent has a boolean data type (true or false). Please re-enter parseContent: " + webClientCommunicator.getParseContent());
	}

	/**
	 * Test enter parseContent is false
	 *
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK
	 */
	@Test
	void testParseContentIsFalse() {
		webClientCommunicator.setURI("/device-parse-content-false");
		webClientCommunicator.setParseContent("false");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();

		assertEquals("200 OK", stats.get("URI Status"));
	}
}
