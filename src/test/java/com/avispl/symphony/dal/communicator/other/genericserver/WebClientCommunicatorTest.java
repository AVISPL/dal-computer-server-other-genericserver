/*
 * Copyright (c) 2015-2021 AVI-SPL, Inc. All Rights Reserved.
 */
package com.avispl.symphony.dal.communicator.other.genericserver;

import static com.github.tomakehurst.wiremock.core.WireMockConfiguration.options;
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
 */
public class WebClientCommunicatorTest {
	private static final int HTTP_PORT = 8088;
	private static final int HTTPS_PORT = 8443;
	private static final String HOST_NAME = "127.0.0.1";
	private static final String PROTOCOL = "http";
	static WebClientCommunicator webClientCommunicator;

	@Rule
	public WireMockRule wireMockRule = new WireMockRule(options().port(HTTP_PORT).httpsPort(HTTPS_PORT)
			.bindAddress(HOST_NAME));

	@BeforeEach
	public void init() throws Exception {
		wireMockRule.start();
		webClientCommunicator = new WebClientCommunicator();
		webClientCommunicator.setTrustAllCertificates(true);
		webClientCommunicator.setProtocol(PROTOCOL);
		webClientCommunicator.setPort(wireMockRule.port());
		webClientCommunicator.setHost(HOST_NAME);
		webClientCommunicator.setContentType("text/plain");
		webClientCommunicator.setAuthenticationScheme(HttpCommunicator.AuthenticationScheme.None);
		webClientCommunicator.init();
	}

	@AfterEach
	public void stopWireMockRule() {
		webClientCommunicator.destroy();
		wireMockRule.stop();
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 200 OK for HTML content
	 */
	@Test
	public void getMultipleStatisticsWithHtmlPage() throws Exception {
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
	public void getMultipleStatisticsWithXMLFile() throws Exception {
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
	public void getMultipleStatisticsWithJpgFile() throws Exception {
		// Attempt to check accessible for a jpg file
		webClientCommunicator.setURI("/jpg-file.jpg");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code 403 Forbidden.
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode403() throws Exception {
		webClientCommunicator.setURI("/forbidden");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("403 Forbidden", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with 404 response status code.
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode404() throws Exception {
		webClientCommunicator.setURI("/not-exist-uri");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("404 Not Found", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with default case that does not configure URI.
	 */
	@Test
	public void getMultipleStatisticsWithDefault() throws Exception {
		// should be "Not Configured" when the URI are not set in Adapter Properties
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("Not Configured", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 401 Unauthorized
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode401() throws Exception {
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
	public void getMultipleStatisticsWithStatusCode300() throws Exception {
		webClientCommunicator.setURI("/redirect");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("300 Multiple Choice", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 500 Internal Server Error
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode500() throws Exception {
		webClientCommunicator.setURI("/internal-server-error");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("500 Internal Server Error", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 501 Not Implemented
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode501() throws Exception {
		webClientCommunicator.setURI("/not-implemented");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("501 Not Implemented", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 502 Bad Gateway
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode502() throws Exception {
		webClientCommunicator.setURI("/bad-gateway");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("502 Bad Gateway", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 503 Service Unavailable
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode503() throws Exception {
		webClientCommunicator.setURI("/service-unavailable");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("503 Service Unavailable", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 504 Gateway Timeout
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode504() throws Exception {
		webClientCommunicator.setURI("/gateway-timeout");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("504 Gateway Timeout", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 505 HTTP Version Not Supported
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode505() throws Exception {
		webClientCommunicator.setURI("/http-version-not-supported");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("505 HTTP Version Not Supported", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 506 Variant Also Negotiates
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode506() throws Exception {
		webClientCommunicator.setURI("/variant-also-negotiates");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("506 Variant Also Negotiates", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 507 Insufficient Storage (WebDAV)
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode507() throws Exception {
		webClientCommunicator.setURI("/insufficient-storage");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("507 Insufficient Storage (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 508 Loop Detected (WebDAV)
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode508() throws Exception {
		webClientCommunicator.setURI("/loop-detected");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("508 Loop Detected (WebDAV)", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 510 Not Extended
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode510() throws Exception {
		webClientCommunicator.setURI("/not-extended");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("510 Not Extended", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response 511 Network Authentication Required
	 */
	@Test
	public void getMultipleStatisticsWithStatusCode511() throws Exception {
		webClientCommunicator.setURI("/network-authentication-required");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("511 Network Authentication Required", stats.get("URI Status"));
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and full path URI.
	 */
	@Test
	public void getMultipleStatisticsWithFullPath() throws Exception {
		//  Expect 200 OK for full path of URI
		webClientCommunicator.setURI("http://localhost:8088/");
		ExtendedStatistics extendedStatistics = (ExtendedStatistics) webClientCommunicator.getMultipleStatistics().get(0);
		Map<String, String> stats = extendedStatistics.getStatistics();
		assertEquals("200 OK", stats.get("URI Status"));
	}

	@Test
	public void getMultipleStatisticsWithAPIError() throws IOException {
		webClientCommunicator.setURI("/out-range-http-status-code");
		assertThrows(ResourceNotReachableException.class, () -> webClientCommunicator.getMultipleStatistics(), "Expect fail here due to status code 600 out of range 200");
	}

	/**
	 * Test method for {@link WebClientCommunicator#getMultipleStatistics()} with response status code as 200 and https protocol being used.
	 */
	@Test
	public void getMultipleStatisticsWithHttpsProtocol() throws Exception {
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
}
